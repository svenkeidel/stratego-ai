{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Fix where

import Prelude hiding ((.))

import Control.Arrow hiding (loop)

import Data.Order

lfp :: (ArrowChoice c, Eq a, BoundedLattice a) => c a a -> c () a
lfp f = proc () -> loop -< bot
  where
    loop = proc x -> do
      x' <- f -< x
      if x' == x
        then returnA -< x
        else loop -< x'

class Arrow c => ArrowFix c where
  fixA :: BoundedLattice (c x y) => (c x y -> c x y) -> c x y
  -- fixA :: BoundedLattice (c x x) => c x x -> c () x
