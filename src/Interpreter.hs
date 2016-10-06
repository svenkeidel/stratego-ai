{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Interpreter where

import Prelude hiding (fail)

import Syntax hiding (Fail)

import Control.Arrow (first)
import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.State hiding (fail)
import Control.Monad.Reader hiding (fail)
import Data.Map (Map)

data Interp r s f a = Interp { runInterp :: r -> s -> f (a,s) }

instance Functor f => Functor (Interp r s f) where
  fmap f int = Interp $ \r s -> fmap (first f) (runInterp int r s)

instance Monad f => Applicative (Interp r s f) where
  pure = return
  (<*>) = ap

instance MonadPlus f => Alternative (Interp r s f) where
  empty = mzero
  (<|>) = mplus

instance Monad f => Monad (Interp r s f) where
  return a = Interp $ \_ s -> return (a,s)
  int >>= k = Interp $ \r s -> do
                (a,s') <- runInterp int r s
                runInterp (k a) r s'

instance MonadPlus f => MonadPlus (Interp r s f) where
  mzero = Interp $ \_ _ -> mzero
  mplus m1 m2 = Interp $ \r s -> runInterp m1 r s `mplus` runInterp m2 r s

instance Monad f => MonadState s (Interp r s f) where
  get = Interp $ \_ s -> return (s,s)
  put s = Interp $ \_ _ -> return ((),s)
  state f = Interp $ \_ s -> return (f s)

instance Monad f => MonadReader r (Interp r s f) where
  ask = Interp $ curry return
  local f int = Interp $ \r s -> runInterp int (f r) s
  reader f = Interp $ \r s -> return (f r,s)

try :: (Monad f,Interpreter f) => f a -> (Result a -> f b) -> f b
try a k = result a >>= k

try2 :: (Monad f,Interpreter f) => f a -> f b -> (Result a -> Result b -> f c) -> f c
try2 int1 int2 k = do
  res1 <- result int1
  res2 <- result int2
  k res1 res2

class Interpreter f where
  success :: a -> f a
  fail :: f a
  result :: f a -> f (Result a)

instance (Functor f,Interpreter f) =>  Interpreter (Interp r s f) where
  success a = Interp $ \_ s -> success (a,s)
  fail = Interp $ \_ _ -> fail
  result int = Interp $ \r s -> do
                 let go res = case res of
                       Success (a,s') -> (Success a,s')
                       Fail -> (Fail,s)
                 fmap go $ result $ runInterp int r s

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

instance Interpreter Result where
  success = Success
  fail = Fail
  result = return

type StratEnv = Map Var Strat
