module Result where

import Control.Monad

data Result a = Success a | Fail
  deriving (Eq,Show)

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
