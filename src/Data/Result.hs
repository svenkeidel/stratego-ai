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

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess Fail = False

instance Functor Result where
  fmap = map

map :: ArrowChoice c => c x y -> c (Result x) (Result y)
map f = proc r -> case r of
  Success a -> Success ^<< f -< a
  Fail -> returnA -< Fail

instance Applicative Result where
  pure = return
  (<*>) = ap

instance Monad Result where
  return = Success
  f >>= k = case f of
    Success a -> k a
    Fail -> Fail
  fail _ = Fail

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

instance (PreOrd a p, ArrowChoice p) => PreOrd (Result a) p where
  (⊑) = proc m -> case m of
    (Success a, Success b) -> (⊑) -< (a,b)
    (Fail, Fail) -> returnA -< True
    (_,_) -> returnA -< False

instance (PartOrd a p, ArrowChoice p) => PartOrd (Result a) p

instance (Galois x y p, ArrowChoice p) => Galois (Result x) (Result y) p where
  alpha = map alpha
  gamma = map gamma
