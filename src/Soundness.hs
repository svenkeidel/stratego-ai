{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Soundness where

import           Prelude hiding (abs)
import           InterpreterArrow

import           Control.Arrow

import           Data.HashSet (HashSet)
import           Data.Hashable
import           Data.Powerset (Pow)
import           Data.Order
import           Data.Foldable (toList)

import           Test.QuickCheck hiding (Result(..))
import           Text.Printf

import           Utils

sound :: ( Galois (Pow (x,(s,ts))) (x',(s',ts')) (->),
           Galois (Pow (m (y,(s,ts)))) (m' (y',(s',ts'))) (->),
           PreOrd (m' (y',(s',ts'))) (->),
           Show (Pow (m (y,(s,ts)))), Show (m' (y',(s',ts')))
         )
      => r -> Interp r s ts m x y -> Interp r s' ts' m' x' y' -> Pow (x,(s,ts)) -> Property
sound r ceval aeval = proc xs -> do
  abs <- runInterp aeval r <<< alpha -< xs
  con <- alpha <<< fmap (runInterp ceval r) -< xs
  res <- (⊑) -< (con,abs)
  returnA -< counterexample (printf "%s < %s" (show con) (show abs)) res

isWittness :: (PartOrd t p, ArrowChoice p, ArrowApply p, Hashable t, Eq t) => p (t, HashSet t) Bool
isWittness = proc (t,ts) -> anyA (proc t' -> do b <- (⊑) -< (t,t'); returnA -< t /= t' && b) -<< toList ts



