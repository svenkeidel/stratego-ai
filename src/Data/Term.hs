{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
module Data.Term where

import Prelude hiding (fail)

import Data.Constructor
import Data.Text(Text)

import Control.Arrow
import Control.Arrow.Try

type Ar c = (ArrowChoice c, ArrowTry c, ArrowPlus c)
class IsTerm t where
  matchTermAgainstConstructor :: Ar c => c ([t'],[t]) [t] -> c (Constructor, [t'], t) t 
  matchTermAgainstString :: Ar c => c (Text,t) t
  matchTermAgainstNumber :: Ar c => c (Int,t) t
  matchTermAgainstExplode :: Ar c => c t t -> c t t -> c t t
  equal :: Ar c => c (t,t) t
  convertFromList :: Ar c => c (t,t) t
  mapSubterms :: Ar c => c [t] [t] -> c t t

  cons :: Ar c => c (Constructor,[t]) t
  numberLiteral :: Ar c => c Int t
  stringLiteral :: Ar c => c Text t

class IsTerm t => IsAbstractTerm t where
  wildcard :: Ar c => c () t

class TermUtils t where
  size :: t -> Int
  height :: t -> Int
  convertToList :: [t] -> t
