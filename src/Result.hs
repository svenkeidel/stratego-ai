module Result where

import Control.Monad
import Data.Hashable
import Data.Semigroup

data Result a = Success a | Fail
  deriving (Eq,Ord,Show)

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
