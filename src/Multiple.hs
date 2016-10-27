{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Multiple where

import Prelude hiding (fail,concat)

import Interpreter hiding (Result(..))
import qualified Interpreter as I

import Control.Applicative
import Control.Monad hiding (fail)

import Data.Foldable (toList)

data Multiple f a
  = Success (f a)
  | Fail
  | SuccessOrFail (f a)
  | Bottom

instance (Functor f,Foldable f,Show a) => Show (Multiple f a) where
  show (Success s) = "Success\n" ++ unlines (toList (fmap show s))
  show (SuccessOrFail s) = "SuccessOrFail\n" ++ unlines (toList (fmap show s))
  show Fail = "Fail"
  show Bottom = "Bottom"
     
instance (Functor f) => Functor (Multiple f) where
  fmap f (Success s) = Success (fmap f s)
  fmap _ Fail = Fail
  fmap f (SuccessOrFail s) = SuccessOrFail (fmap f s)
  fmap _ Bottom = Bottom

instance (Foldable f,MonadPlus f) => Applicative (Multiple f) where
  pure = return
  (<*>) = ap

instance (Foldable f,MonadPlus f) => Alternative (Multiple f) where
  empty = mzero
  (<|>) = mplus

instance (Foldable f,MonadPlus f) => Monad (Multiple f) where
  return = Success . return
  Success l >>= k = msum (fmap k l)
  SuccessOrFail l >>= k = case msum (fmap k l) of
    SuccessOrFail a -> SuccessOrFail a
    Success a -> SuccessOrFail a
    Fail -> Fail
    Bottom -> Fail
  Fail >>= _ = Fail
  Bottom >>= _ = Bottom

instance (Foldable f,MonadPlus f) => MonadPlus (Multiple f) where
  mzero = Bottom
  mplus (Success x) (Success y) = Success (x `mplus` y)
  mplus (Success x) Fail = SuccessOrFail x
  mplus Fail (Success y) = SuccessOrFail y
  mplus Fail Fail = Fail
  mplus (SuccessOrFail x) (Success y) = SuccessOrFail (x `mplus` y)
  mplus (Success x) (SuccessOrFail y) = SuccessOrFail (x `mplus` y)
  mplus (SuccessOrFail x) Fail = SuccessOrFail x
  mplus Fail (SuccessOrFail y) = SuccessOrFail y
  mplus (SuccessOrFail x) (SuccessOrFail y) = SuccessOrFail (x `mplus` y)
  mplus Bottom x = x
  mplus x Bottom = x

instance (Monad f) => CanFail (Multiple f) where
  success = Success . return
  fail = Fail

instance (Foldable f,MonadPlus f) => Try (Multiple f) a where
  try (Success a) k = msum (fmap (k . I.Success) a)
  try Fail k = k I.Fail
  try (SuccessOrFail a) k = try (Success a) k `mplus` k I.Fail
  try Bottom _ = Bottom
