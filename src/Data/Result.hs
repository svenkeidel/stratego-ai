module Data.Result where

import Control.Monad
import Control.Applicative

import Data.Hashable
import Data.Semigroup
import Data.Order

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

instance PreOrd a => PreOrd (Result a) where
  Fail ⊑ Fail = True
  Success a ⊑ Success b = a ⊑ b
  _ ⊑ _ = False

instance PartOrd a => PartOrd (Result a)
