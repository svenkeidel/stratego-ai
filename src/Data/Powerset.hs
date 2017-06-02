{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Powerset where

import           Control.Arrow 

import           Data.Sequence (Seq,(|>))
import           Data.Hashable
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Foldable (foldl')
import           Data.Order

newtype Pow a = Pow (Seq a) deriving (Functor, Applicative, Monad, Monoid, Foldable)

union :: Pow a -> Pow a -> Pow a
union = mappend

instance (Eq a, Hashable a) => PreOrd (Pow a) where
  as ⊑ bs = toHashSet as ⊑ toHashSet bs

instance (Eq a, Hashable a) => PartOrd (Pow a)

instance (Eq a, Hashable a) => Lattice (Pow a) where
  (⊔) = union

class Arrow p => Deduplicate p where
  dedup :: (Hashable b,Eq b) => p a b -> p a b

toHashSet :: (Hashable a, Eq a) => Pow a -> HashSet a
toHashSet (Pow as) = foldl' (flip H.insert) H.empty as

fromFoldable :: Foldable f => f a -> Pow a
fromFoldable = Pow . foldl' (|>) mempty 

dedup' :: (Eq a, Hashable a) => Pow a -> Pow a
dedup' = fromFoldable . toHashSet
