module Data.TypedResult where

import Control.Monad
import Data.Hashable
import Data.Semigroup
import Data.Text (Text)

data TypedResult a = Success a | Fail | TypeError Text
  deriving (Eq,Ord,Show)

instance Functor TypedResult where
  fmap f (Success a) = Success (f a)
  fmap _ Fail = Fail
  fmap _ (TypeError t) = TypeError t

instance Applicative TypedResult where
  pure = return
  (<*>) = ap

instance Monad TypedResult where
  return = Success
  f >>= k = case f of
    Success a -> k a
    Fail -> Fail
    TypeError t -> TypeError t

instance Semigroup (TypedResult a) where
  Success a <> _ = Success a
  Fail <> Success b = Success b
  Fail <> Fail = Fail
  TypeError t <> _ = TypeError t
  _ <> TypeError t = TypeError t

instance Monoid (TypedResult a) where
  mempty = Fail
  mappend = (<>)

instance Hashable a => Hashable (TypedResult a) where
  hashWithSalt s (Success a) = s `hashWithSalt` (0::Int) `hashWithSalt` a
  hashWithSalt s Fail = s `hashWithSalt` (1::Int)
  hashWithSalt s (TypeError t) = s `hashWithSalt` (2::Int) `hashWithSalt` t
