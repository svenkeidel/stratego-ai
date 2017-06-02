module Data.UncertainResult where

import Prelude hiding (fail,concat)

import Control.Monad
import Control.Applicative

import Data.Order

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
  fail _ = Fail

instance Alternative UncertainResult where
  empty = mzero
  (<|>) = mplus

instance MonadPlus UncertainResult where
  mzero = Fail
  mplus (Success a) _ = Success a
  mplus (SuccessOrFail a) _ = SuccessOrFail a
  mplus Fail r = r

successOrFail :: UncertainResult a -> UncertainResult a
successOrFail r = case r of
  Success x -> SuccessOrFail x
  SuccessOrFail x -> SuccessOrFail x
  Fail -> Fail

instance PreOrd a => PreOrd (UncertainResult a) where
  Fail ⊑ Fail = True
  Success a ⊑ Success b = a ⊑ b
  Success a ⊑ SuccessOrFail b = a ⊑ b
  Fail ⊑ SuccessOrFail _ = True
  _ ⊑ _ = False

instance PartOrd a => PartOrd (UncertainResult a)

instance Lattice a => Lattice (UncertainResult a) where
  Success x ⊔ Success y = Success (x ⊔ y)
  Success x ⊔ Fail = SuccessOrFail x
  Fail ⊔ Success y = SuccessOrFail y
  Fail ⊔ Fail = Fail
  SuccessOrFail x ⊔ Success y = SuccessOrFail (x ⊔ y)
  Success x ⊔ SuccessOrFail y = SuccessOrFail (x ⊔ y)
  SuccessOrFail x ⊔ Fail = SuccessOrFail x
  Fail ⊔ SuccessOrFail y = SuccessOrFail y
  SuccessOrFail x ⊔ SuccessOrFail y = SuccessOrFail (x ⊔ y)

