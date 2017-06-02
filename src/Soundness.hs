{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Soundness where

import           Prelude hiding (abs)
import           InterpreterArrow

import           Data.HashSet (HashSet)
import           Data.Hashable
import           Data.Powerset (Pow)
import           Data.Order

import           Test.QuickCheck hiding (Result(..))
import           Text.Printf


sound :: ( Galois (Pow (x,s)) (x',s'), Galois (Pow (m (y,s))) (m' (y',s')),
           Show (Pow (m (y,s))), Show (m' (y',s'))
         , PreOrd (m' (y',s'))
         )
      => r -> Pow (x,s) -> Interp r s m x y -> Interp r s' m' x' y' -> Property
sound r xs ceval aeval =
  let abs = runInterp aeval r (alpha xs)
      con = alpha (fmap (runInterp ceval r) xs)
  in counterexample (printf "%s < %s" (show abs) (show abs))
       (con ⊑ abs)

isWittness :: (PartOrd t, Hashable t, Eq t) => t -> HashSet t -> Bool
isWittness t = any (\t' -> t /= t' && t ⊑ t')



