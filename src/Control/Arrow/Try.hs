{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Control.Arrow.Try where

import Prelude hiding (id,(.))

import Control.Category
import Control.Arrow

import Data.Order
import Data.Complete

class Arrow c => ArrowTry c where
  fail :: c x y
  try :: Lattice (Complete z) c => c x y -> c y z -> c x z -> c x z

instance ArrowTry (->) where
  fail = error "fail"
  try f g _ = g . f

success :: ArrowTry p => p a a
success = id
{-# INLINE success #-}

instance ArrowTry (Kleisli Maybe) where
  fail = Kleisli $ const Nothing
  try e s f = Kleisli $ \a ->
                case runKleisli e a of
                  Just b -> runKleisli s b
                  Nothing -> runKleisli f a

instance ArrowTry (Kleisli []) where
  fail = Kleisli $ const []
  try e s f = Kleisli $ \a ->
                case runKleisli e a of
                  [] -> runKleisli f a
                  bs -> bs >>= runKleisli s
