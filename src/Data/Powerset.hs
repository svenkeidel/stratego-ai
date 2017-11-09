{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Powerset where

import           Prelude hiding (map,(.),id)

import           Control.Category
import           Control.Arrow 
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Deduplicate

import           Data.Sequence (Seq,(|>),(<|),viewl,ViewL(..))
import qualified Data.Sequence as S
import           Data.Hashable
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Foldable (foldl',toList)
-- import           Data.Order
import           Data.List (intercalate)
 
newtype Pow a = Pow {unPow :: Seq a} deriving (Eq, Functor, Applicative, Monad, Alternative, MonadPlus, Monoid, Foldable, Traversable)

map :: ArrowChoice c => c x y -> c (Pow x) (Pow y)
map f = proc (Pow s) -> case viewl s of
  EmptyL -> returnA -< Pow S.empty
  x S.:< xs -> do
    y <- f -< x
    Pow ys <- map f -< Pow xs
    returnA -< Pow (y <| ys)

singleton :: a -> Pow a
singleton x = Pow (S.singleton x)

union :: Pow a -> Pow a -> Pow a
union = mappend

unit :: Arrow c => c a (Pow a)
unit = arr return

instance Show a => Show (Pow a) where
  show (Pow a) = "{" ++ intercalate ", " (show <$> toList a) ++ "}"

cartesian :: (Pow a, Pow b) -> Pow (a,b)
cartesian (as,bs) = do
  a <- as
  b <- bs
  return (a,b)

toHashSet :: (Hashable a, Eq a) => Pow a -> HashSet a
toHashSet (Pow as) = foldl' (flip H.insert) H.empty as
{-# INLINE toHashSet #-}

fromFoldable :: Foldable f => f a -> Pow a
fromFoldable = Pow . foldl' (|>) mempty 
{-# INLINE fromFoldable #-}

size :: Pow a -> Int
size (Pow s) = S.length s
           
instance MonadDeduplicate Pow where
  dedup = fromFoldable . toHashSet

