{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Single where

import Prelude hiding (fail,concat)

import Interpreter hiding (Result(..))
import qualified Interpreter as I

import Control.Applicative
import Control.Monad hiding (fail)

import Data.Semigroup

data Single a
  = Success a
  | Fail
  | SuccessOrFail a
  | Bottom

instance (Show a) => Show (Single a) where
  show (Success s) = "Success\n" ++ show s
  show (SuccessOrFail s) = "SuccessOrFail\n" ++ show s
  show Fail = "Fail"
  show Bottom = "Bottom"

instance Functor Single where
  fmap f (Success a) = Success (f a)
  fmap _ Fail = Fail
  fmap f (SuccessOrFail a) = SuccessOrFail (f a)
  fmap _ Bottom = Bottom


instance Applicative Single where
  pure = return
  (<*>) = ap

instance Alternative Single where
  empty = undefined
  (<|>) = undefined

instance Monad Single where
  return = Success
  Success l >>= k = k l
  SuccessOrFail l >>= k = k l
  Fail >>= _ = Fail
  Bottom >>= _ = Bottom

instance Semigroup a => Semigroup (Single a) where
  Success x <> Success y = Success (x <> y)
  Success x <> Fail = SuccessOrFail x
  Fail <> Success y = SuccessOrFail y
  Fail <> Fail = Fail
  SuccessOrFail x <> Success y = SuccessOrFail (x <> y)
  Success x <> SuccessOrFail y = SuccessOrFail (x <> y)
  SuccessOrFail x <> Fail = SuccessOrFail x
  Fail <> SuccessOrFail y = SuccessOrFail y
  SuccessOrFail x <> SuccessOrFail y = SuccessOrFail (x <> y)
  Bottom <> x = x
  x <> Bottom = x

instance Semigroup a => Monoid (Single a) where
  mempty = Bottom
  mappend = (<>)

instance CanFail Single where
  success = Success
  fail = Fail

instance Semigroup a => Try Single a where
  try (Success a) k = k (I.Success a)
  try Fail k = k I.Fail
  try (SuccessOrFail a) k = try (Success a) k <> k I.Fail
  try Bottom _ = Bottom
