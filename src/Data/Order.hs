{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Order where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable

-- | A preorder is reflexive and transitive
class PreOrd a where
  (⊑) :: a -> a -> Bool

-- | A partial order is preorder which is aditionally anti-symmetric
class PreOrd a => PartOrd a

-- | A lattice is a partial order in which all pairs of elements have a unique least upper bound
class PartOrd a => Lattice a where
  (⊔) :: a -> a -> a

lub :: (Lattice a, Foldable f) => f a -> a
lub = foldl1 (⊔)

instance (PreOrd x, PreOrd y) => PreOrd (x,y) where
  (x1,y1) ⊑ (x2,y2) = x1 ⊑ x2 && y1 ⊑ y2

instance (PartOrd x, PartOrd y) => PartOrd (x,y)

instance (Lattice x, Lattice y) => Lattice (x,y) where
  (x1,y1) ⊔ (x2,y2) = (x1 ⊔ x2, y1 ⊔ y2)

instance (Eq a, Hashable a) => PreOrd (HashSet a) where
  xs ⊑ ys = all (`HS.member` ys) xs

instance (Eq a, Hashable a) => PartOrd (HashSet a)

instance (Eq a, Hashable a) => Lattice (HashSet a) where
  (⊔) = HS.union

instance PreOrd a => PreOrd [a] where
  (a:as) ⊑ (b:bs) = a ⊑ b && as ⊑ bs
  [] ⊑ [] = True
  _ ⊑ _ = False

instance PartOrd a => PartOrd [a] where

instance Lattice a => Lattice [a] where
  (x:xs) ⊔ (y:ys) = x⊔y : xs⊔ys
  [] ⊔ [] = []
  _ ⊔ _ = error "Top"

instance PreOrd a => PreOrd (Maybe a) where
  Just x ⊑ Just y = x ⊑ y
  Nothing ⊑ Nothing = True
  _ ⊑ _ = False

instance PartOrd a => PartOrd (Maybe a)

instance Lattice a => Lattice (Maybe a) where
  Just x ⊔ Just y = Just (x⊔y)
  Nothing ⊔ Nothing = Nothing
  _ ⊔ _ = error "Top"

-- | A galois connection consisting of an abstraction function alpha
-- and a concretization function gamma between two pre-ordered sets
-- has to satisfy forall x,y. alpha x ⊑ y iff x ⊑ gamma y
class (PreOrd x, PreOrd y) => Galois x y where
  alpha :: x -> y
  gamma :: y -> x

instance (Galois x x', Galois y y') => Galois (x,y) (x',y') where
  alpha (x,y) = (alpha x, alpha y)
  gamma (x,y) = (gamma x, gamma y)


