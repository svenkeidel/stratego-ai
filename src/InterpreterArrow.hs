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
import           Stack

import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Join
import           Control.Arrow.Try
import           Control.Arrow.Fix
import           Control.Category
import           Control.Monad hiding (fail)

import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.Monoid
import           Data.Order
import           Data.Complete
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

newtype Interp r s ts m a b = Interp { runInterp :: r -> (a,(s,ts)) -> m (b,(s,ts)) }

instance Monad m => Category (Interp r s ts m) where
  id = Interp $ \_ a -> return a
  {-# INLINE id #-}
  Interp f . Interp g = Interp $ \r -> g r >=> f r
  {-# INLINE (.) #-}

instance Monad m => Arrow (Interp r s ts m) where
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

instance Monad m => ArrowChoice (Interp r s ts m) where
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

instance Monad m => ArrowApply (Interp r s ts m) where
  app = Interp $ \r ((f,b),e) -> runInterp f r (b,e)
  {-# INLINE app #-}

instance Monad m => HasStratEnv (Interp StratEnv ts s m) where
  readStratEnv = Interp $ \r (_,e) -> return (r,e)
  {-# INLINE readStratEnv #-}
  localStratEnv (Interp f) = Interp $ \_ ((a,r),e) -> f r (a,e)
  {-# INLINE localStratEnv #-}

instance Monad m => HasStratEnv (Interp (r,StratEnv) s ts m) where
  readStratEnv = Interp $ \(_,r) (_,e) -> return (r,e)
  {-# INLINE readStratEnv #-}
  localStratEnv (Interp f) = Interp $ \(r1,_) ((a,r2),e) -> f (r1,r2) (a,e)
  {-# INLINE localStratEnv #-} 

instance Monad m => HasSignature (Interp (Signature,r) s ts m) where
  getSignature = Interp $ \(r,_) (_,e) -> return (r,e)
  {-# INLINE getSignature #-} 

instance Monad m => ArrowFix (Interp r (ConcreteTermEnv t) ts m) where
  fixA n f = f (fixA n f)
  fixA' n m f = f (fixA' n m f,fixA' n m f)

instance Monad m => ArrowFix (Interp r (AbstractTermEnv t) ts m) where
  fixA n f
    | n <= 0    = top
    | otherwise = f (fixA (n-1) f)

  fixA' m0 n0 f = go m0 n0
    where
      go 0 _ = top
      go m 0 = go (m-1) n0
      go m n = f (go (m-1) n0,go m (n-1))

instance (Monad m,
          IsTerm t (Interp r (ConcreteTermEnv t) ts m),
          ArrowTry (Interp r (ConcreteTermEnv t) ts m)) =>
  IsTermEnv (ConcreteTermEnv t) t (Interp r (ConcreteTermEnv t) ts m) where
  getTermEnv = Interp $ \_ (_,(e,ts)) -> return (e,(e,ts))
  {-# INLINE getTermEnv #-} 
  putTermEnv = Interp $ \_ (e,(_,ts)) -> return ((),(e,ts))
  {-# INLINE putTermEnv #-} 
  lookupTermVar f g = proc v -> do
    ConcreteTermEnv env <- getTermEnv -< ()
    case M.lookup v env of
      Just t -> f -< t
      Nothing -> g -< ()
  {-# INLINE lookupTermVar #-} 
  insertTerm = proc (v,t) -> do
    ConcreteTermEnv env <- getTermEnv -< ()
    putTermEnv -< ConcreteTermEnv (M.insert v t env)
  {-# INLINE insertTerm #-} 
  deleteTermVars = proc vars -> do
    ConcreteTermEnv e <- getTermEnv -< ()
    putTermEnv -< ConcreteTermEnv (foldr' M.delete e vars)
  {-# INLINE deleteTermVars #-} 
  unionTermEnvs = arr (\(vars, ConcreteTermEnv e1, ConcreteTermEnv e2) ->
    ConcreteTermEnv (M.union e1 (foldr' M.delete e2 vars)))
  {-# INLINE unionTermEnvs #-} 

instance (Monad m, Lattice t (Interp r (AbstractTermEnv t) ts m),
          IsAbstractTerm t (Interp r (AbstractTermEnv t) ts m),
          ArrowTry (Interp r (AbstractTermEnv t) ts m),
          ArrowJoin (Interp r (AbstractTermEnv t) ts m)) =>
  IsTermEnv (AbstractTermEnv t) t (Interp r (AbstractTermEnv t) ts m) where
  getTermEnv = Interp $ \_ (_,(e,ts)) -> return (e,(e,ts))
  {-# INLINE getTermEnv #-} 
  putTermEnv = Interp $ \_ (e,(_,ts)) -> return ((),(e,ts))
  {-# INLINE putTermEnv #-} 
  lookupTermVar f g = proc v -> do
    AbstractTermEnv env <- getTermEnv -< ()
    case M.lookup v env of
      Just t -> f -< t
      Nothing -> (f <<< wildcard) <+> g -< ()
  {-# INLINE lookupTermVar #-} 
  insertTerm = proc (v,t) -> do
    AbstractTermEnv env <- getTermEnv -< ()
    putTermEnv -< AbstractTermEnv (M.insert v t env)
  {-# INLINE insertTerm #-} 
  deleteTermVars = proc vars -> do
    AbstractTermEnv e <- getTermEnv -< ()
    putTermEnv -< AbstractTermEnv (foldr' M.delete e vars)
  {-# INLINE deleteTermVars #-} 
  unionTermEnvs = arr (\(vars,AbstractTermEnv e1, AbstractTermEnv e2) ->
    AbstractTermEnv (M.union e1 (foldr' M.delete e2 vars)))
  {-# INLINE unionTermEnvs #-} 

instance ArrowTry (Interp r s ts Result) where
  fail = Interp $ \_ _ -> R.Fail
  {-# INLINE fail #-} 
  try (Interp f) (Interp g) (Interp h) = Interp $ \e a ->
    case f e a of
      R.Success b -> g e b 
      R.Fail -> h e a
  {-# INLINE try #-} 

instance ArrowJoin (Interp r s ts Result) where
  Interp f <+> Interp g = Interp $ \r a -> f r a <> g r a 
  {-# INLINE (<+>) #-} 
  alternatives = Interp $ \_ (as,e) -> (,e) <$> msum (fmap return as)
  {-# INLINE alternatives #-} 

-- instance Lattice s (Interp r s ts UncertainResult) => ArrowTry (Interp r s ts UncertainResult) where
--   fail = Interp $ \_ _ -> U.Fail
--   try (Interp f) (Interp g) (Interp h) = Interp $ \r (a,e) ->
--     case f r (a,e) of
--       U.Success b -> g r b 
--       U.SuccessOrFail b' -> join $ uncomplete . fst <$> runInterp (⊔) r ((complete (g r b'), complete (h r (a,e))),e)
--       U.Fail -> h r (a,e)

-- complete :: UncertainResult (x,e) -> UncertainResult (Complete x, e)
-- complete = fmap (first Complete)

-- uncomplete :: UncertainResult (Complete x,e) -> UncertainResult (x, e)
-- uncomplete c = case c of
--   U.Success (Complete x,e) -> U.Success (x,e)
--   U.SuccessOrFail (Complete x,e) -> U.SuccessOrFail (x,e)
--   _ -> U.Fail
      

-- instance Lattice s (Interp r s ts UncertainResult) => ArrowJoin (Interp r s ts UncertainResult) where
--   Interp f <+> Interp g = Interp $ \r x@(_,e) ->
--     join $ uncomplete . fst <$> runInterp (⊔) r ((complete (f r x), complete (g r x)),e)
--   alternatives = Interp $ \_ (as,e) -> (,e) <$> msum (fmap return as)

instance Deduplicate (Interp r s ts Result) where
  dedup f = f
  {-# INLINE dedup #-} 

instance Deduplicate (Interp r s ts UncertainResult) where
  dedup f = f
  {-# INLINE dedup #-} 

instance ArrowTry (Interp r s ts PowersetResult) where
  fail = Interp $ \_ _ -> P.PowRes (return R.Fail)
  {-# INLINE fail #-} 
  try (Interp f) (Interp g) (Interp h) = Interp $ \r a -> P.PowRes $ do
    b <- P.unPowRes $ f r a
    case b of
      R.Success b' -> P.unPowRes $ g r b'
      R.Fail -> P.unPowRes $ h r a
  {-# INLINE try #-} 

instance ArrowJoin (Interp r s ts PowersetResult) where
  Interp f <+> Interp g = Interp $ \r x -> f r x `P.union` g r x
  {-# INLINE (<+>) #-} 
  alternatives = Interp $ \_ (as,e) -> P.fromFoldable (fmap (return . (,e)) as)
  {-# INLINE alternatives #-} 
                 
instance TypeError (Interp r s ts PowersetResult) where
  typeError = Interp $ \_ _ -> mempty
  {-# INLINE typeError #-} 

instance (Eq s, Hashable s, Eq ts, Hashable ts) => Deduplicate (Interp r s ts PowersetResult) where
  dedup (Interp f) = Interp $ \r a -> P.dedup' $ f r a
  {-# INLINE dedup #-} 

instance ArrowTry (Interp r s ts TypedResult) where
  fail = Interp $ \_ _ -> T.Fail
  {-# INLINE fail #-} 
  try (Interp f) (Interp g) (Interp h) = Interp $ \e a ->
    case f e a of
      T.Success b -> g e b 
      T.Fail -> h e a
      T.TypeError t -> T.TypeError t
  {-# INLINE try #-} 

instance ArrowJoin (Interp r s ts TypedResult) where
  Interp f <+> Interp g = Interp $ \x -> f x `mappend` g x
  alternatives = Interp $ \_ (as,e) -> (,e) <$> msum (fmap return as)

instance TypeError (Interp r s ts TypedResult) where
  typeError = Interp $ \_ (e,_) -> T.TypeError e


data Stack ts addr cell = Stack { timestamp :: ts, store :: M.HashMap addr cell, stacked :: [addr] }

instance (Hashable addr, Eq addr, Monad m, BoundedLattice cell (Interp r s (Stack ts addr cell) m))
         => HasStack ts addr cell (Interp r s (Stack ts addr cell) m) where
  getTimeStamp = Interp $ \_ (_,(e,st)) -> return (timestamp st,(e,st))
  putTimeStamp = Interp $ \_ (ts,(e,st)) -> return ((),(e, st{timestamp=ts}))

  readAddr = Interp $ \_ (addr,(e,st)) -> return (M.lookup addr (store st), (e,st))
  writeAddr = Interp $ \_ ((addr,cell),(e,st)) -> return ((), (e, st{store=M.insert addr cell (store st)}))

  stack = Interp $ \_ (_,(e,st)) -> return (stacked st,(e,st))
  localPush addr f = Interp $ \r (x,(e,st)) -> do
    let addrs = stacked st
    let pushed = st{stacked=addr:addrs}
    (y,(e',st')) <- runInterp f r (x,(e,pushed))
    let popped = st'{stacked=addrs}
    return (y,(e',popped))
