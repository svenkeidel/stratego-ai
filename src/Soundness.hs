{-# LANGUAGE FlexibleContexts #-}
module Soundness where

import           Data.Powerset (Pow)
import           Data.Order
import           ConcreteSemantics

import           Test.QuickCheck hiding (Result(..))

class Soundness c' where
  sound :: (Galois (Pow x) x', Galois (Pow y) y') => Interp x y -> c' x' y' -> Property



