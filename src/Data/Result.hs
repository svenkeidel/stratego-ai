{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
module Data.Result where

import Prelude hiding (map)

import Control.Monad
import Control.Applicative
import Control.Arrow

import Data.Hashable
import Data.Semigroup
import Data.Order

data Result a = Success a | Fail
  deriving (Eq,Ord,Show)

map :: ArrowChoice c => c x y -> c (Result x) (Result y)
map f = proc r -> case r of
  Success a -> Success ^<< f -< a
  Fail -> returnA -< Fail

instance Functor Result where
  fmap f (Success a) = Success (f a)
  fmap _ Fail = Fail

instance Applicative Result where
  pure = return
  (<*>) = ap

instance Monad Result where
  return = Success
  f >>= k = case f of
    Success a -> k a
    Fail -> Fail
  fail _ = Fail
           
instance Foldable Result where
  foldMap f (Success a) = f a 
  foldMap _ Fail        = mempty 

instance Traversable Result where
  traverse f (Success a) = Success <$> f a
  traverse _ Fail        = pure Fail

instance Alternative Result where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Result where
  mzero = Fail
  mplus (Success a) _ = Success a
  mplus Fail r = r

instance Semigroup (Result a) where
  Success a <> _ = Success a
  Fail <> Success b = Success b
  Fail <> Fail = Fail

instance Monoid (Result a) where
  mempty = Fail
  mappend = (<>)

instance Hashable a => Hashable (Result a) where
  hashWithSalt s (Success a) = s `hashWithSalt` (0::Int) `hashWithSalt` a
  hashWithSalt s Fail = s `hashWithSalt` (1::Int)

instance PreOrd a => PreOrd (Result a) where
  x ⊑ y = case (x,y) of
    (Success a, Success b) -> a ⊑ b
    (Fail, Fail) -> True
    (_,_) -> False

instance (PartOrd a) => PartOrd (Result a)

instance Galois x y => Galois (Result x) (Result y) where
  alpha = fmap alpha
  gamma = fmap gamma
