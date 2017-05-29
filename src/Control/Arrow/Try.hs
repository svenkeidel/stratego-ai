{-# LANGUAGE FlexibleInstances #-}
module Control.Arrow.Try where

import Prelude hiding (id,(.))

import Control.Category
import Control.Arrow

class Arrow p => ArrowTry p where
  fail :: p t a
  try :: Monoid c => p a b -> p b c -> p a c ->  p a c

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
