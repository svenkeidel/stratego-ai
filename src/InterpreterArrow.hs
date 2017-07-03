{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE UndecidableInstances #-}
module InterpreterArrow where

import           Prelude hiding ((.),id,fail)
    
import           Syntax (StratEnv,HasStratEnv(..))
import           Signature hiding (Top)

import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Append
import           Control.Arrow.Try
import           Control.Category
import           Control.Monad hiding (fail)

import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.Monoid
import           Data.Order
import           Data.Complete
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
  Interp f . Interp g = Interp $ \r -> g r >=> f r

instance Monad m => Arrow (Interp r s m) where
  arr f = Interp (\_ (a,e) -> return (f a, e))
  first (Interp f) = Interp $ \r ((a,b),e) -> fmap (\(c,e') -> ((c,b),e')) (f r (a,e))
  second (Interp f) = Interp $ \r ((a,b),e) -> fmap (\(c,e') -> ((a,c),e')) (f r (b,e))
  Interp f *** Interp g = Interp $ \r ((a,b),e) -> do
    (c,e')  <- f r (a,e)
    (d,e'') <- g r (b,e')
    return ((c,d),e'')
  Interp f &&& Interp g = Interp $ \r (a,e) -> do
    (b,e')  <- f r (a,e)
    (c,e'') <- g r (a,e')
    return ((b,c),e'')

instance Monad m => ArrowChoice (Interp r s m) where
  left (Interp f) = Interp $ \r (a,e) -> case a of
    Left b -> first Left <$> f r (b,e)
    Right c -> return (Right c,e)
  right (Interp f) = Interp $ \r (a,e) -> case a of
    Left c -> return (Left c,e)
    Right b -> first Right <$> f r (b,e)
  Interp f +++ Interp g = Interp $ \r (a,e) -> case a of
    Left b -> first Left  <$> f r (b,e)
    Right c -> first Right <$> g r (c,e)
  Interp f ||| Interp g = Interp $ \r (a,e) -> case a of
    Left b -> f r (b,e)
    Right c -> g r (c,e)

instance Monad m => ArrowApply (Interp r s m) where
  app = Interp $ \r ((f,b),e) -> runInterp f r (b,e)

instance Monad m => HasStratEnv (Interp StratEnv s m) where
  readStratEnv = Interp $ \r (_,e) -> return (r,e)
  localStratEnv (Interp f) = Interp $ \_ ((a,r),e) -> f r (a,e)

instance Monad m => HasStratEnv (Interp (r,StratEnv) s m) where
  readStratEnv = Interp $ \(_,r) (_,e) -> return (r,e)
  localStratEnv (Interp f) = Interp $ \(r1,_) ((a,r2),e) -> f (r1,r2) (a,e)

instance Monad m => HasSignature (Interp (Signature,r) s m) where
  getSignature = Interp $ \(r,_) (_,e) -> return (r,e)

instance (Monad m,
          HasTerm t (Interp r (ConcreteTermEnv t) m),
          ArrowTry (Interp r (ConcreteTermEnv t) m)) =>
  HasTermEnv (ConcreteTermEnv t) t (Interp r (ConcreteTermEnv t) m) where
  getTermEnv = Interp $ \_ (_,e) -> return (e,e)
  putTermEnv = Interp $ \_ (e,_) -> return ((),e)
  lookupTermVar = proc v -> do
    ConcreteTermEnv env <- getTermEnv -< ()
    returnA -< M.lookup v env
  insertTerm = proc (v,t) -> do
    ConcreteTermEnv env <- getTermEnv -< ()
    putTermEnv -< ConcreteTermEnv (M.insert v t env)
  deleteTermVars = proc vars -> do
    ConcreteTermEnv e <- getTermEnv -< ()
    putTermEnv -< ConcreteTermEnv (foldr' M.delete e vars)
  unionTermEnvs = arr (\(vars, ConcreteTermEnv e1, ConcreteTermEnv e2) ->
    ConcreteTermEnv (M.union e1 (foldr M.delete e2 vars)))

instance (Monad m, Lattice t (Interp r (AbstractTermEnv t) m),
          HasTerm t (Interp r (AbstractTermEnv t) m),
          ArrowTry (Interp r (AbstractTermEnv t) m),
          ArrowAppend (Interp r (AbstractTermEnv t) m)) =>
  HasTermEnv (AbstractTermEnv t) t (Interp r (AbstractTermEnv t) m) where
  getTermEnv = Interp $ \_ (_,e) -> return (e,e)
  putTermEnv = Interp $ \_ (e,_) -> return ((),e)
  lookupTermVar = proc v -> do
    AbstractTermEnv env <- getTermEnv -< ()
    case M.lookup v env of
      Just t -> returnA -< Just t
      Nothing -> (returnA -< Nothing) <+> (Just ^<< wildcard -< ())
  insertTerm = proc (v,t) -> do
    AbstractTermEnv env <- getTermEnv -< ()
    putTermEnv -< AbstractTermEnv (M.insert v t env)
  deleteTermVars = proc vars -> do
    AbstractTermEnv e <- getTermEnv -< ()
    putTermEnv -< AbstractTermEnv (foldr' M.delete e vars)
  unionTermEnvs = arr (\(vars,AbstractTermEnv e1, AbstractTermEnv e2) ->
    AbstractTermEnv (M.union e1 (foldr M.delete e2 vars)))

instance ArrowTry (Interp r s Result) where
  fail = Interp $ \_ _ -> R.Fail
  try (Interp f) (Interp g) (Interp h) = Interp $ \e a ->
    case f e a of
      R.Success b -> g e b 
      R.Fail -> h e a

instance ArrowAppend (Interp r s Result) where
  Interp f <+> Interp g = Interp $ \r a -> f r a <> g r a 
  alternatives = Interp $ \_ (as,e) -> (,e) <$> msum (fmap return as)

instance Lattice s (Interp r s UncertainResult) => ArrowTry (Interp r s UncertainResult) where
  fail = Interp $ \_ _ -> U.Fail
  try (Interp f) (Interp g) (Interp h) = Interp $ \r (a,e) ->
    case f r (a,e) of
      U.Success b -> g r b 
      U.SuccessOrFail b' -> join $ uncomplete . fst <$> runInterp (⊔) r ((complete (g r b'), complete (h r (a,e))),e)
      U.Fail -> h r (a,e)

complete :: UncertainResult (x,e) -> UncertainResult (Complete x, e)
complete = fmap (first Complete)

uncomplete :: UncertainResult (Complete x,e) -> UncertainResult (x, e)
uncomplete c = case c of
  U.Success (Complete x,e) -> U.Success (x,e)
  U.SuccessOrFail (Complete x,e) -> U.SuccessOrFail (x,e)
  _ -> U.Fail
      

instance Lattice s (Interp r s UncertainResult) => ArrowAppend (Interp r s UncertainResult) where
  Interp f <+> Interp g = Interp $ \r x@(_,e) ->
    join $ uncomplete . fst <$> runInterp (⊔) r ((complete (f r x), complete (g r x)),e)
  alternatives = Interp $ \_ (as,e) -> (,e) <$> msum (fmap return as)

instance Deduplicate (Interp r s UncertainResult) where
  dedup f = f

instance ArrowTry (Interp r s PowersetResult) where
  fail = Interp $ \_ _ -> P.PowRes (return R.Fail)
  try (Interp f) (Interp g) (Interp h) = Interp $ \r a -> P.PowRes $ do
    b <- P.unPowRes $ f r a
    case b of
      R.Success b' -> P.unPowRes $ g r b'
      R.Fail -> P.unPowRes $ h r a

instance ArrowAppend (Interp r s PowersetResult) where
  Interp f <+> Interp g = Interp $ \r x -> f r x `P.union` g r x
  alternatives = Interp $ \_ (as,e) -> P.fromFoldable (fmap (return . (,e)) as)

instance TypeError (Interp r s PowersetResult) where
  typeError = Interp $ \_ _ -> mempty

instance (Eq s, Hashable s) => Deduplicate (Interp r s PowersetResult) where
  dedup (Interp f) = Interp $ \r a -> P.dedup' $ f r a

instance ArrowTry (Interp r s TypedResult) where
  fail = Interp $ \_ _ -> T.Fail
  try (Interp f) (Interp g) (Interp h) = Interp $ \e a ->
    case f e a of
      T.Success b -> g e b 
      T.Fail -> h e a
      T.TypeError t -> T.TypeError t

instance ArrowAppend (Interp r s TypedResult) where
  Interp f <+> Interp g = Interp $ \x -> f x `mappend` g x
  alternatives = Interp $ \_ (as,e) -> (,e) <$> msum (fmap return as)

instance TypeError (Interp r s TypedResult) where
  typeError = Interp $ \_ (e,_) -> T.TypeError e
