{-# LANGUAGE FlexibleContexts #-}
module Control.Arrow.Fix where

import Prelude hiding ((.))

import Control.Arrow hiding (loop)

import Data.Order

class Arrow c => ArrowFix c where
  fixA :: BoundedLattice (c x y) => (c x y -> c x y) -> c x y
