{-# LANGUAGE FlexibleContexts #-}
module Control.Arrow.Fix where

import Control.Arrow

import Data.Order

class Arrow c => ArrowFix c where
  fixA :: BoundedLattice y => Int -> (c x y -> c x y) -> c x y
  fixA' :: BoundedLattice y => Int -> Int -> ((c x y,c x y) -> c x y) -> c x y
