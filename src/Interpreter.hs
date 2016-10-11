{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module Interpreter where

import Prelude hiding (fail)

import Syntax hiding (Fail)

import Control.Arrow (first)
import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.State hiding (fail)
import Control.Monad.Reader hiding (fail)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Lazy.Merge as M

data Interp s f a = Interp { runInterp :: StratEnv -> (Int,Int) -> s -> f (a,s) }

instance Functor f => Functor (Interp s f) where
  fmap f int = Interp $ \r i s -> fmap (first f) (runInterp int r i s)

instance Monad f => Applicative (Interp s f) where
  pure = return
  (<*>) = ap

instance MonadPlus f => Alternative (Interp s f) where
  empty = mzero
  (<|>) = mplus

instance Monad f => Monad (Interp s f) where
  return a = Interp $ \_ _ s -> return (a,s)
  int >>= k = Interp $ \r i s -> do
                (a,s') <- runInterp int r i s
                runInterp (k a) r i s'

instance MonadPlus f => MonadPlus (Interp s f) where
  mzero = Interp $ \_ _ _ -> mzero
  mplus m1 m2 = Interp $ \r i s -> runInterp m1 r i s `mplus` runInterp m2 r i s

instance Monad f => MonadState s (Interp s f) where
  get = Interp $ \_ _ s -> return (s,s)
  put s = Interp $ \_ _ _ -> return ((),s)
  state f = Interp $ \_ _ s -> return (f s)

instance Monad f => MonadReader StratEnv (Interp s f) where
  ask = Interp $ \r _ s -> return (r,s)
  local f int = Interp $ \r i s -> runInterp int (f r) i s
  reader f = Interp $ \r _ s -> return (f r,s)

class CanFail f where
  success :: a -> f a
  fail :: f a
  try :: f a -> (Result a -> f b) -> f b

instance (CanFail f) => CanFail (Interp s f) where
  success a = Interp $ \_ _ s -> success (a,s)
  fail = Interp $ \_ _ _ -> fail
  try int k =
    Interp $ \r i s -> try (runInterp int r i s) $ \res ->
      case res of
        Fail -> runInterp (k Fail) r i s
        Success (b,s') -> runInterp (k (Success b)) r i s'

recursionDepth :: Monad f => Interp s f Int
recursionDepth = Interp $ \_ (i,_) s -> return (i,s)

recursionLimit :: Monad f => Interp s f Int
recursionLimit = Interp $ \_ (_,l) s -> return (l,s)

decent :: Interp s f a -> Interp s f a
decent int = Interp $ \r (i,l) s -> runInterp int r (i+1,l) s

data Result t = Success t | Fail deriving Show

instance Functor Result where
  fmap f (Success x) = Success (f x)
  fmap _ Fail = Fail

instance Applicative Result where
  pure = return
  (<*>) = ap

instance Alternative Result where
  empty = mzero
  (<|>) = mplus

instance Monad Result where
  return = Success
  Success a >>= k = k a
  Fail >>= _ = Fail

instance MonadPlus Result where
  mzero = Fail
  Success x `mplus` _ = Success x
  _ `mplus` Success x = Success x
  Fail `mplus` Fail = Fail

instance CanFail Result where
  success = Success
  fail = Fail
  try r f = f r

test :: (CanFail f) => (Strat -> t -> Interp env f t)
     -> Strat -> t -> Interp env f t
test f s t = try (f s t) $ \r -> case r of
    Success _ -> success t
    Fail -> fail
{-# INLINE test #-}

neg :: (Monad f,CanFail f) => (Strat -> t -> Interp env f t)
    -> Strat -> t -> Interp env f t
neg f s t = do
  env <- get
  try (f s t) $ \r -> case r of
    Success _ -> fail
    Fail -> do
      put env
      success t
{-# INLINE neg #-}

sequence :: (Monad f) => (Strat -> t -> Interp env f t)
         -> Strat -> Strat -> t -> Interp env f t
sequence f s1 s2 t = f s1 t >>= f s2
{-# INLINE sequence #-}

choice :: (MonadPlus f) => (Strat -> t -> Interp env f t)
       -> Strat -> Strat -> t -> Interp env f t
choice f s1 s2 t = f s1 t `mplus` f s2 t
{-# INLINE choice #-}

leftChoice :: (CanFail f) => (Strat -> t -> Interp env f t)
           -> Strat -> Strat -> t -> Interp env f t
leftChoice f s1 s2 t =
  try (f s1 t) $ \r1 ->
  try (f s2 t) $ \r2 ->
    case (r1,r2) of
      (Success x,_) -> success x
      (Fail,Success x) -> success x
      (Fail,Fail) -> fail
{-# INLINE leftChoice #-}

recur :: (Monad f) => (Strat -> t -> Interp env f t)
      -> StratVar -> Strat -> t -> Interp env f t
recur f x s t = local (M.insert x s) (f s t)
{-# INLINE recur #-}

limit :: (Monad f,CanFail f) => Interp env f t -> t -> Interp env f t
limit f top = do
  depth <- recursionDepth
  lim <- recursionLimit
  if depth < lim
     then decent f
     else success top
{-# INLINE limit #-}

var :: Monad f => (Strat -> t -> Interp env f t)
    -> StratVar -> t -> Interp env f t
var f x t = do
  m <- reader (M.lookup x)
  case m of
    Just s -> f s t
    Nothing -> error "Recursion variable was not in scope"
{-# INLINE var #-}

scope :: Monad f => (Strat -> t -> Interp (Map TermVar a) f t)
      -> [TermVar] -> Strat -> t -> Interp (Map TermVar a) f t
scope f vars s t = do
    env <- get
    put (foldr M.delete env vars)
    t' <- f s t
    env' <- get
    put $ merge env' env
    return t'
  where
    merge = M.merge M.preserveMissing M.dropMissing $ M.zipWithMatched $ \k x y ->
            if k `elem` vars then y else x
{-# INLINE scope #-}

path :: (Monad f,CanFail f) => (Strat -> t -> Interp env f t)
     -> Int -> Strat -> Constructor -> [t] -> Interp env f (Constructor,[t])
path f i s c ts = (c,) <$> nth i (f s) ts
{-# INLINE path #-}

cong :: (Monad f,CanFail f) => (Strat -> t -> Interp env f t)
     -> Constructor -> [Strat] -> Constructor -> [t] -> Interp env f (Constructor,[t])
cong f c ss0 c' ts0
  | c /= c' || length ts0 /= length ss0 = fail
  | otherwise =
      let go (s:ss) (t:ts) = do
            t' <- f s t
            ts' <- go ss ts
            return (t':ts')
          go _ _ = return []
      in (c,) <$> go ss0 ts0
{-# INLINE cong #-}

one :: (MonadPlus f,CanFail f) => (Strat -> t -> Interp env f t)
    -> Strat -> Constructor -> [t] -> Interp env f (Constructor,[t])
one f s c ts = msum [(c,) <$> nth i (f s) ts | i <- [1..length ts]]
{-# INLINE one #-}

some :: (MonadPlus f,CanFail f) => (Strat -> t -> Interp env f t)
     -> Strat -> Constructor -> [t] -> Interp env f (Constructor,[t])
some f s c ts0 =
  let go (t:ts) = try (f s t) $ \m -> case m of
        Success t' -> do
          (_,ts') <- go ts
          success (True,t':ts')
        Fail -> do
          (b,ts') <- go ts
          success (b,t:ts')
      go [] = success (False,[])
  in do
      (oneSucceeded,ts') <- go ts0
      if oneSucceeded
        then success (c,ts')
        else fail
{-# INLINE some #-}

all :: (MonadPlus f,CanFail f) => (Strat -> t -> Interp env f t)
    -> Strat -> Constructor -> [t] -> Interp env f (Constructor,[t])
all f s c ts0 =
  let go (t:ts) = do
        t' <- f s t
        ts' <- go ts
        success (t':ts')
      go [] = return []
  in (c,) <$> go ts0
{-# INLINE all #-}

nth :: (Monad f,CanFail f) => Int -> (a -> Interp s f a) -> ([a] -> Interp s f [a])
nth 1 f (x:xs) = do
  x' <- f x
  return (x':xs)
nth i f (x:xs) = do
  xs' <- nth (i-1) f xs
  return (x:xs')
nth _ _ [] = fail

