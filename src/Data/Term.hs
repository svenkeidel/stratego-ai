{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE Arrows #-}
-- {-# LANGUAGE OverloadedStrings #-}
module Data.Term where

import Prelude hiding (fail)

import Data.Constructor
import Data.Text(Text)
import Data.Order
import Data.Complete

import Control.Arrow hiding ((<+>))
import Control.Arrow.Try
import Control.Arrow.Join

-- import Utils

type (:+:) = Either
infixr :+:

class (PartOrd t c, Lattice (Complete t) c, ArrowChoice c) => IsTerm t c where

  matchTermAgainstConstructor :: (ArrowTry c, ArrowJoin c) => c ([t'],[t]) [t] -> c (Constructor, [t'], t) t 
  matchTermAgainstString :: (ArrowTry c, ArrowJoin c) => c (Text,t) t
  matchTermAgainstNumber :: (ArrowTry c, ArrowJoin c) => c (Int,t) t
  matchTermAgainstExplode :: (ArrowJoin c,ArrowTry c) => c t t -> c t t -> c t t
  equal :: (ArrowChoice c, ArrowJoin c, ArrowTry c) => c (t,t) t
  convertFromList :: (ArrowChoice c, ArrowTry c, ArrowJoin c) => c (t,t) t
  lift :: (ArrowChoice c, ArrowTry c, ArrowJoin c, IsTerm t c) => c [t] [t] -> c t t

  cons :: Arrow c => c (Constructor,[t]) t
  numberLiteral :: Arrow c => c Int t
  stringLiteral :: Arrow c => c Text t

class IsTerm t c => IsAbstractTerm t c where
  wildcard :: Arrow c => c () t

class TermUtils t where
  size :: t -> Int
  height :: t -> Int
  convertToList :: [t] -> t

-- size :: IsTerm t p => p t Int
-- size = proc t -> do
--   t' <- matchTerm -< t
--   case t' of
--     Cons _ ts ->
--       arr (succ . sum) <<< mapA size -< ts
--     _ -> returnA -< 1
         
-- height :: IsTerm t p => p t Int
-- height = proc t -> do
--   t' <- matchTerm -< t
--   case t' of
--     Cons _ ts
--       | null ts -> returnA -< 1
--       | otherwise -> arr (succ . maximum) <<< mapA height -< ts
--     _ -> returnA -< 1

