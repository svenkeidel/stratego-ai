{-# LANGUAGE FlexibleContexts #-}
module Signature(Signature, Sort(..), Fun(..), SortId(..), empty, insertType, lookupType, insertSubtype, subtype, lubs) where

import           Prelude hiding (lookup)

import           Sort
import           SubtypeRelation (SubtypeRelation)
import qualified SubtypeRelation as R

import           Data.Constructor
import           Data.HashSet (HashSet)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M

type Context = HashMap Constructor Fun
data Signature = Signature Context SubtypeRelation deriving (Show,Eq)
data Fun = Fun [Sort] Sort deriving (Show,Eq)

empty :: Signature
empty = Signature M.empty R.empty

insertType :: Constructor -> Fun -> Signature -> Signature
insertType con ty (Signature ctx sub) = Signature (M.insert con ty ctx) sub

insertSubtype :: SortId -> SortId -> Signature -> Signature
insertSubtype ty1 ty2 (Signature ctx sub) = Signature ctx (R.insert ty1 ty2 sub)

lookupType :: Constructor -> Signature -> Maybe Fun
lookupType c (Signature sig _) = M.lookup c sig

lubs :: Signature -> Sort -> Sort -> HashSet Sort
lubs (Signature _ rel) = R.lubs rel

subtype :: Signature -> Sort -> Sort -> Bool
subtype (Signature _ rel) = R.subtype rel
