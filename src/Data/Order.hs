{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Order where

import Prelude hiding ((.),map)

import qualified Data.HashSet as HS
import Data.Hashable
import Data.Complete
import Data.Powerset
import qualified Data.Sequence as S

-- REMINDER TO SELF: DO NOT ADD EXTRA PARAMETER FOR STATIC INFORMATION TO ORDERING FUNCTION!!!
-- IT MAKES THE CODE MUCH MORE COMPLICATED.

-- | Reflexive, transitive order
class PreOrd x where
  (⊑) :: x -> x -> Bool

-- | Reflexive, transitive and anti-symmetric order
class PreOrd x => PartOrd x

-- | Reflexive, transitive, anti-symmetric and complete order
class PartOrd x => Lattice x where
  (⊔) :: x -> x -> x

class Lattice x => BoundedLattice x where
  top :: x
  bot :: x

lub :: Lattice x => Pow x -> x
lub (Pow xs) = go xs
  where
    go s = case S.viewl s of
      S.EmptyL -> error "no lub of empty set"
      a S.:< as
        | null as -> a
        | otherwise ->
          a ⊔ go as

instance (Eq a, Hashable a) => PreOrd (Pow a) where
  as ⊑ bs = all (`HS.member` toHashSet as) (toHashSet bs)

instance (Eq a, Hashable a) => PartOrd (Pow a) 

instance (Eq a, Hashable a) => Lattice (Pow a) where
  as ⊔ bs = as `union` bs

instance (Eq (x,y), Hashable (x,y), Galois (Pow x) x', Galois (Pow y) y')
  => Galois (Pow (x,y)) (x',y') where
  alpha m = (alpha (fst <$> m),alpha (snd <$> m))
  gamma m = cartesian (gamma (fst m),gamma (snd m))

instance PreOrd x => PreOrd (Complete x) where
  c1 ⊑ c2 = case (c1,c2) of
    (_,Top) -> True
    (Complete x, Complete y) -> x ⊑ y
    (_,_) -> False

instance PartOrd x => PartOrd (Complete x) where

instance (PartOrd x, Lattice (Complete x),
          PartOrd y, Lattice (Complete y)) => Lattice (Complete (x,y)) where
  c ⊔ d = case (c,d) of
    (Complete (x1,y1), Complete (x2,y2)) ->
      let (z1,z2) = (Complete x1,Complete y1) ⊔ (Complete x2,Complete y2)
      in (,) <$> z1 <*> z2
    _ -> Top

instance (PreOrd a,PreOrd b) => PreOrd (a,b) where
  (a1,b1) ⊑ (a2,b2) = a1 ⊑ a2 && b1 ⊑ b2

instance (PartOrd a, PartOrd b) => PartOrd (a,b)

instance (Lattice a, Lattice b) => Lattice (a,b) where
  (a1,b1) ⊔ (a2,b2) = (a1 ⊔ a2,b1 ⊔ b2)
 
instance PreOrd a => PreOrd [a] where
  l1 ⊑ l2 = case (l1,l2) of
    (a:as,b:bs) -> a ⊑ b && as ⊑ bs
    ([],[]) -> True
    (_,_) -> False

instance PartOrd a => PartOrd [a] where

-- wrapComplete :: ArrowChoice c => c (x,y) (Complete z) -> c (Complete x, Complete y) (Complete z)
-- wrapComplete f = proc xs -> case xs of
--   (Complete x, Complete y) -> f -< (x,y)
--   _ -> returnA -< Top

instance (PartOrd x, Lattice (Complete x)) => Lattice (Complete [x]) where
  l1 ⊔ l2 = case (l1,l2) of
    (Complete (a: as), Complete (b:bs)) ->
      let c  = Complete a ⊔ Complete b
          cs = Complete as ⊔ Complete bs
      in (:) <$> c <*> cs
    (Complete [], Complete []) -> Complete []
    (_,_) -> Top

instance PreOrd a => PreOrd (Maybe a) where
  m1 ⊑ m2 = case (m1,m2) of
    (Just x,Just y) -> x ⊑ y
    (Nothing, Nothing) -> True
    (_,_) -> False

instance PartOrd a => PartOrd (Maybe a)

-- instance (PartOrd a c, Lattice (Complete a) c, ArrowChoice c) => Lattice (Complete (Maybe a)) c where
--   (⊔) = wrapComplete $ proc m -> case m of
--     (Just x,Just y) -> fmap Just ^<< (⊔) -< (Complete x,Complete y)
--     (Nothing, Nothing) -> returnA -< Complete Nothing
--     (_,_) -> returnA -< Top

-- -- instance (Eq a, Hashable a, PartOrd a c, PartOrd a' c, ArrowChoice c, Galois (Pow a) a' c)
-- --     => Galois (Pow (Maybe a)) (Complete (Maybe a')) c where
-- --   alpha = proc xs ->
-- --     if xs == fromFoldable [Nothing]
-- --     then returnA -< Complete Nothing
-- --     else map (Just ^<< alpha) -< traverse maybeComplete xs
-- --     where
-- --       maybeComplete :: Maybe a -> Complete a
-- --       maybeComplete (Just a) = Complete a
-- --       maybeComplete Nothing  = Top
-- --   gamma = undefined

instance PreOrd Int where
  x ⊑ y = x <= y

instance PartOrd Int

instance PreOrd () where
  () ⊑ () = True

instance PartOrd ()

instance Lattice () where
  () ⊔ () = ()

instance Lattice (Complete ()) where
  c1 ⊔ c2 = case (c1,c2) of
    (_,Top) -> Top
    (Top,_) -> Top
    (Complete (),Complete ()) -> Complete ()

-- -- instance (Arrow p, ArrowZero p) => Lattice (Complete Int) p where
-- --   (⊔) = proc (x,y) ->
-- --     if x == y
-- --       then returnA -< x
-- --       else zeroArrow -< ()

-- | A galois connection consisting of an abstraction function alpha
-- and a concretization function gamma between two pre-ordered sets
-- has to satisfy forall x,y. alpha x ⊑ y iff x ⊑ gamma y
class (PreOrd x, PreOrd y) => Galois x y where
  alpha :: x -> y
  gamma :: y -> x

instance (Galois x x', Galois y y') => Galois (x,y) (x',y') where
  alpha (x,y) = (alpha x, alpha y)
  gamma (x',y') = (gamma x', gamma y')
