{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Soundness where

import           Prelude hiding (abs)
import           InterpreterArrow

import           Data.Powerset (Pow)
import           Data.Order
import           Data.Hashable

import           Test.QuickCheck hiding (Result(..))
import           Text.Printf


-- sound :: (Galois (Pow x) x',
--           Galois (Pow s) s',
--           Galois (Pow (m (y,s))) (m' (y',s')),
--           Eq x, Eq s, Hashable x, Hashable s,
--           Show (Pow (m (y,s))), Show (m' (y',s'))
--          )
--       => r -> Interp r s m x y -> Interp r s' m' x' y' -> Pow (x,s) -> Property
-- sound r ceval aeval xs = do
--   let abs = runInterp aeval r (alpha xs)
--       con = alpha (fmap (runInterp ceval r) xs)
--       res = con ⊑ abs
--   counterexample (printf "abstract %s < concrete %s" (show abs) (show con)) res

-- isWittness :: (PartOrd t p, ArrowChoice p, ArrowApply p, Hashable t, Eq t) => p (t, HashSet t) Bool
-- isWittness = proc (t,ts) -> anyA (proc t' -> do b <- (⊑) -< (t,t'); returnA -< t /= t' && b) -<< toList ts



