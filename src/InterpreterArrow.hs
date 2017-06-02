{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Arrows #-}
module InterpreterArrow where

import           Prelude hiding ((.),id,fail)
    
import           Syntax (StratEnv,HasStratEnv(..))
import           Signature

import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Append
import           Control.Arrow.Try
import           Control.Category
import           Control.Monad hiding (fail)

import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.Monoid
import           Data.Order
import           Data.Powerset (Deduplicate(..))
import           Data.PowersetResult (PowersetResult)
import qualified Data.PowersetResult as P
import           Data.Result (Result)
import qualified Data.Result as R
import           Data.Foldable
import           Data.Term
import           Data.TermEnv
import           Data.TypedResult (TypedResult,TypeError(..))
import qualified Data.TypedResult as T
import           Data.UncertainResult (UncertainResult)
import qualified Data.UncertainResult as U

newtype Interp r s m a b = Interp { runInterp :: r -> (a,s) -> m (b,s) }

instance Monad m => Category (Interp r s m) where
  id = Interp $ \_ a -> return a
  {-# INLINE id #-}
  Interp f . Interp g = Interp $ \r -> g r >=> f r
  {-# INLINE (.) #-}

instance Monad m => Arrow (Interp r s m) where
  arr f = Interp (\_ (a,e) -> return (f a, e))
  {-# INLINE arr #-}
  first (Interp f) = Interp $ \r ((a,b),e) -> fmap (\(c,e') -> ((c,b),e')) (f r (a,e))
  {-# INLINE first #-}
  second (Interp f) = Interp $ \r ((a,b),e) -> fmap (\(c,e') -> ((a,c),e')) (f r (b,e))
  {-# INLINE second #-}
  Interp f *** Interp g = Interp $ \r ((a,b),e) -> do
    (c,e')  <- f r (a,e)
    (d,e'') <- g r (b,e')
    return ((c,d),e'')
  {-# INLINE (***) #-}
  Interp f &&& Interp g = Interp $ \r (a,e) -> do
    (b,e')  <- f r (a,e)
    (c,e'') <- g r (a,e')
    return ((b,c),e'')
  {-# INLINE (&&&) #-}

instance Monad m => ArrowChoice (Interp r s m) where
  left (Interp f) = Interp $ \r (a,e) -> case a of
    Left b -> first Left <$> f r (b,e)
    Right c -> return (Right c,e)
  {-# INLINE left #-}
  right (Interp f) = Interp $ \r (a,e) -> case a of
    Left c -> return (Left c,e)
    Right b -> first Right <$> f r (b,e)
  {-# INLINE right #-}
  Interp f +++ Interp g = Interp $ \r (a,e) -> case a of
    Left b -> first Left  <$> f r (b,e)
    Right c -> first Right <$> g r (c,e)
  {-# INLINE (+++) #-}
  Interp f ||| Interp g = Interp $ \r (a,e) -> case a of
    Left b -> f r (b,e)
    Right c -> g r (c,e)
  {-# INLINE (|||) #-}

instance Monad m => ArrowApply (Interp r s m) where
  app = Interp $ \r ((f,b),e) -> runInterp f r (b,e)
  {-# INLINE app #-}

instance Monad m => HasStratEnv (Interp StratEnv s m) where
  readStratEnv = Interp $ \r (_,e) -> return (r,e)
  {-# INLINE readStratEnv #-}
  localStratEnv (Interp f) = Interp $ \_ ((a,r),e) -> f r (a,e)
  {-# INLINE localStratEnv #-}

instance Monad m => HasStratEnv (Interp (r,StratEnv) s m) where
  readStratEnv = Interp $ \(_,r) (_,e) -> return (r,e)
  {-# INLINE readStratEnv #-}
  localStratEnv (Interp f) = Interp $ \(r1,_) ((a,r2),e) -> f (r1,r2) (a,e)
  {-# INLINE localStratEnv #-}

instance Monad m => HasSignature (Interp (Signature,r) s m) where
  getSignature = Interp $ \(r,_) (_,e) -> return (r,e)

instance (Monad m,
          HasTerm t (Interp r (ConcreteTermEnv t) m),
          ArrowTry (Interp r (ConcreteTermEnv t) m)) =>
  HasTermEnv (ConcreteTermEnv t) t (Interp r (ConcreteTermEnv t) m) where
  getTermEnv = Interp $ \_ (_,e) -> return (e,e)
  {-# INLINE getTermEnv #-}
  putTermEnv = Interp $ \_ (e,_) -> return ((),e)
  {-# INLINE putTermEnv #-}
  lookupTermVar = proc v -> do
    ConcreteTermEnv env <- getTermEnv -< ()
    returnA -< M.lookup v env
  {-# INLINE lookupTermVar #-}
  insertTerm = proc (v,t) -> do
    ConcreteTermEnv env <- getTermEnv -< ()
    putTermEnv -< ConcreteTermEnv (M.insert v t env)
  {-# INLINE insertTerm #-}
  deleteTermVars = proc vars -> do
    ConcreteTermEnv e <- getTermEnv -< ()
    putTermEnv -< ConcreteTermEnv (foldr' M.delete e vars)
  {-# INLINE deleteTermVars #-}
  unionTermEnvs = arr (\(ConcreteTermEnv e1, ConcreteTermEnv e2) -> ConcreteTermEnv (M.union e1 e2))
  {-# INLINE unionTermEnvs #-}

instance (Monad m, Lattice t, Lattice (Maybe t),
          HasTerm t (Interp r (AbstractTermEnv t) m),
          ArrowTry (Interp r (AbstractTermEnv t) m),
          ArrowAppend (Interp r (AbstractTermEnv t) m)) =>
  HasTermEnv (AbstractTermEnv t) t (Interp r (AbstractTermEnv t) m) where
  getTermEnv = Interp $ \_ (_,e) -> return (e,e)
  {-# INLINE getTermEnv #-}
  putTermEnv = Interp $ \_ (e,_) -> return ((),e)
  {-# INLINE putTermEnv #-}
  lookupTermVar = proc v -> do
    AbstractTermEnv env <- getTermEnv -< ()
    case M.lookup v env of
      Just t -> returnA -< Just t
      Nothing -> fail <+> success -< Nothing
  {-# INLINE lookupTermVar #-}
  insertTerm = proc (v,t) -> do
    AbstractTermEnv env <- getTermEnv -< ()
    putTermEnv -< AbstractTermEnv (M.insert v t env)
  {-# INLINE insertTerm #-}
  deleteTermVars = proc vars -> do
    AbstractTermEnv e <- getTermEnv -< ()
    putTermEnv -< AbstractTermEnv (foldr' M.delete e vars)
  {-# INLINE deleteTermVars #-}
  unionTermEnvs = arr (\(AbstractTermEnv e1, AbstractTermEnv e2) -> AbstractTermEnv (M.union e1 e2))
  {-# INLINE unionTermEnvs #-}

instance ArrowTry (Interp r s Result) where
  fail = Interp $ \_ _ -> R.Fail
  {-# INLINE fail #-}
  try (Interp f) (Interp g) (Interp h) = Interp $ \e a ->
    case f e a of
      R.Success b -> g e b 
      R.Fail -> h e a
  {-# INLINE try #-}

instance ArrowAppend (Interp r s Result) where
  Interp f <+> Interp g = Interp $ \r a -> f r a <> g r a 
  {-# INLINE (<+>) #-}
  alternatives = Interp $ \_ (as,e) -> (,e) <$> msum (fmap return as)
  {-# INLINE alternatives #-}

instance Lattice s => ArrowTry (Interp r s UncertainResult) where
  fail = Interp $ \_ _ -> U.Fail
  {-# INLINE fail #-}
  try (Interp f) (Interp g) (Interp h) = Interp $ \r a ->
    case f r a of
      U.Success b -> g r b 
      U.SuccessOrFail b' -> g r b' ⊔ h r a
      U.Fail -> h r a
  {-# INLINE try #-}

instance Lattice s => ArrowAppend (Interp r s UncertainResult) where
  f <+> g = Interp $ \r a -> runInterp f r a ⊔ runInterp g r a
  {-# INLINE (<+>) #-}
  alternatives = Interp $ \_ (as,e) -> (,e) <$> msum (fmap return as)
  {-# INLINE alternatives #-}

instance Deduplicate (Interp r s UncertainResult) where
  dedup f = f
  {-# INLINE dedup #-}

instance ArrowTry (Interp r s PowersetResult) where
  fail = Interp $ \_ _ -> P.PowRes (return R.Fail)
  {-# INLINE fail #-}
  try (Interp f) (Interp g) (Interp h) = Interp $ \r a -> P.PowRes $ do
    b <- P.unPowRes $ f r a
    case b of
      R.Success b' -> P.unPowRes $ g r b'
      R.Fail -> P.unPowRes $ h r a
  {-# INLINE try #-}

instance ArrowAppend (Interp r s PowersetResult) where
  Interp f <+> Interp g = Interp $ \r x -> f r x `P.union` g r x
  {-# INLINE (<+>) #-}
  alternatives = Interp $ \_ (as,e) -> P.fromFoldable (fmap (return . (,e)) as)
  {-# INLINE alternatives #-}

instance TypeError (Interp r s PowersetResult) where
  typeError = Interp $ \_ _ -> mempty

instance (Eq s, Hashable s) => Deduplicate (Interp r s PowersetResult) where
  dedup (Interp f) = Interp $ \r a -> P.dedup' $ f r a
  {-# INLINE dedup #-}

instance ArrowTry (Interp r s TypedResult) where
  fail = Interp $ \_ _ -> T.Fail
  {-# INLINE fail #-}
  try (Interp f) (Interp g) (Interp h) = Interp $ \e a ->
    case f e a of
      T.Success b -> g e b 
      T.Fail -> h e a
      T.TypeError t -> T.TypeError t
  {-# INLINE try #-}

instance ArrowAppend (Interp r s TypedResult) where
  Interp f <+> Interp g = Interp $ \x -> f x `mappend` g x
  {-# INLINE (<+>) #-}
  alternatives = Interp $ \_ (as,e) -> (,e) <$> msum (fmap return as)
  {-# INLINE alternatives #-}

instance TypeError (Interp r s TypedResult) where
  typeError = Interp $ \_ (e,_) -> T.TypeError e
