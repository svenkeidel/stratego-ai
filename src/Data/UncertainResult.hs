module Data.UncertainResult where

import Prelude hiding (fail,concat)
import Control.Monad (ap)

data UncertainResult a
  = Success a
  | Fail
  | SuccessOrFail a

instance Functor UncertainResult where
  fmap f (Success s) = Success (f s)
  fmap _ Fail = Fail
  fmap f (SuccessOrFail s) = SuccessOrFail (f s)
                            
instance Applicative UncertainResult where
  pure = return
  (<*>) = ap

instance Monad UncertainResult where
  return = Success
  Success l >>= k = k l
  SuccessOrFail l >>= k = case k l of
    SuccessOrFail a -> SuccessOrFail a
    Success a -> SuccessOrFail a
    Fail -> Fail
  Fail >>= _ = Fail

instance (Monoid a) => Monoid (UncertainResult a) where
  mempty = SuccessOrFail mempty
  mappend (Success x) (Success y) = Success (x `mappend` y)
  mappend (Success x) Fail = SuccessOrFail x
  mappend Fail (Success y) = SuccessOrFail y
  mappend Fail Fail = Fail
  mappend (SuccessOrFail x) (Success y) = SuccessOrFail (x `mappend` y)
  mappend (Success x) (SuccessOrFail y) = SuccessOrFail (x `mappend` y)
  mappend (SuccessOrFail x) Fail = SuccessOrFail x
  mappend Fail (SuccessOrFail y) = SuccessOrFail y
  mappend (SuccessOrFail x) (SuccessOrFail y) = SuccessOrFail (x `mappend` y)

successOrFail :: UncertainResult a -> UncertainResult a
successOrFail r = case r of
  Success x -> SuccessOrFail x
  SuccessOrFail x -> SuccessOrFail x
  Fail -> Fail
