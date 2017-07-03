{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
module Data.Order where

import Prelude hiding ((.),map)

import Control.Category
import Control.Arrow

import Utils

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Complete
import Data.Powerset hiding (map)
import qualified Data.Sequence as S

-- | Reflexive, transitive order
class Arrow p => PreOrd a p where
  (⊑) :: p (a,a) Bool

-- | Reflexive, transitive and anti-symmetric order
class PreOrd a p => PartOrd a p

-- | Reflexive, transitive, anti-symmetric and complete order
class PartOrd a p => Lattice a p where
  (⊔) :: p (a,a) a

lub :: (Lattice x c, ArrowChoice c) => c (Pow x) x
lub = go . arr (\(Pow a) -> a)
  where
    go = proc s -> case S.viewl s of
      S.EmptyL -> returnA -< error "no lub of empty set"
      x S.:< xs
        | null xs -> returnA -< x
        | otherwise -> do
          y <- go -< xs
          (⊔) -< (x,y)

instance (Eq a, Hashable a, Arrow p) => PreOrd (Pow a) p where
  (⊑) = proc (as,bs) -> (⊑) -< (toHashSet as, toHashSet bs)

instance (Eq a, Hashable a, Arrow p) => PartOrd (Pow a) p

instance (Eq a, Hashable a, Arrow p) => Lattice (Pow a) p where
  (⊔) = arr (uncurry union)

instance (PreOrd x c, ArrowChoice c) => PreOrd (Complete x) c where
  (⊑) = proc (c1,c2) -> case (c1,c2) of
    (_,Top) -> returnA -< True
    (Complete x, Complete y) -> (⊑) -< (x,y)
    (_,_) -> returnA -< False

instance (PartOrd x c, ArrowChoice c) => PartOrd (Complete x) c where

instance (PartOrd x c, Lattice (Complete x) c,
          PartOrd y c, Lattice (Complete y) c,
          ArrowChoice c) => Lattice (Complete (x,y)) c where
  (⊔) = proc (c,d) -> case (c,d) of
    (Complete (x1,y1), Complete (x2,y2)) -> do
      (x3,y3) <- (⊔) -< ((Complete x1,Complete y1),(Complete x2,Complete y2))
      returnA -< (,) <$> x3 <*> y3
    _ -> returnA -< Top

instance (PreOrd a p,PreOrd b p) => PreOrd (a,b) p where
  (⊑) = proc ((a1,b1),(a2,b2)) ->
    (&&) ~<< (⊑) *** (⊑) -< ((a1,a2),(b1,b2))

instance (PartOrd a p, PartOrd b p) => PartOrd (a,b) p

instance (Lattice a p, Lattice b p) => Lattice (a,b) p where
  (⊔) = proc ((a1,b1),(a2,b2)) ->
    (⊔) *** (⊔) -< ((a1,a2),(b1,b2))
 
instance (Eq a, Hashable a, Arrow p) => PreOrd (HashSet a) p where
  (⊑) = arr $ \(xs,ys) -> all (`HS.member` ys) xs

instance (Eq a, Hashable a, Arrow p) => PartOrd (HashSet a) p 

instance (Eq a, Hashable a, Arrow p) => Lattice (HashSet a) p where
  (⊔) = arr $ uncurry HS.union

instance (PreOrd a p, ArrowChoice p)  => PreOrd [a] p where
  (⊑) = proc (l1,l2) -> case (l1,l2) of
    (a:as,b:bs) -> (&&) ~<< (⊑) *** (⊑) -< ((a,b),(as, bs))
    ([],[]) -> returnA -< True
    (_,_) -> returnA -< False

instance (PartOrd a p, ArrowChoice p) => PartOrd [a] p where

wrapComplete :: ArrowChoice c => c (x,y) (Complete z) -> c (Complete x, Complete y) (Complete z)
wrapComplete f = proc xs -> case xs of
  (Complete x, Complete y) -> f -< (x,y)
  _ -> returnA -< Top

instance (PartOrd x c, Lattice (Complete x) c, ArrowChoice c) => Lattice (Complete [x]) c where
  (⊔) = proc (l1,l2) -> case (l1,l2) of
    (Complete (a: as), Complete (b:bs)) -> do
      (c,cs) <- (⊔) *** (⊔) -< ((Complete a,Complete b),(Complete as, Complete bs))
      returnA -< (:) <$> c <*> cs
    (Complete [], Complete []) -> returnA -< Complete []
    (_,_) -> returnA -< Top

instance (PreOrd a p, ArrowChoice p) => PreOrd (Maybe a) p where
  (⊑) = proc m -> case m of
    (Just x,Just y) -> (⊑) -< (x,y)
    (Nothing, Nothing) -> returnA -< True
    (_,_) -> returnA -< False

instance (PartOrd a p, ArrowChoice p) => PartOrd (Maybe a) p

instance (Lattice a p, ArrowChoice p) => Lattice (Complete (Maybe a)) p where
  (⊔) = wrapComplete $ proc m -> case m of
    (Just x,Just y) -> (Complete . Just) ^<< (⊔) -< (x,y)
    (Nothing, Nothing) -> returnA -< Complete Nothing
    (_,_) -> returnA -< Top

instance (Eq a, Hashable a, PartOrd a c, PartOrd a' c, ArrowChoice c, Galois (Pow a) a' c)
    => Galois (Pow (Maybe a)) (Complete (Maybe a')) c where
  alpha = proc xs ->
    if xs == fromFoldable [Nothing]
    then returnA -< Complete Nothing
    else map (Just ^<< alpha) -< traverse maybeComplete xs
    where
      maybeComplete :: Maybe a -> Complete a
      maybeComplete (Just a) = Complete a
      maybeComplete Nothing  = Top
  gamma = undefined

instance Arrow p => PreOrd Int p where
  (⊑) = arr $ uncurry (<=)

instance Arrow p => PartOrd Int p

-- instance (Arrow p, ArrowZero p) => Lattice (Complete Int) p where
--   (⊔) = proc (x,y) ->
--     if x == y
--       then returnA -< x
--       else zeroArrow -< ()

-- | A galois connection consisting of an abstraction function alpha
-- and a concretization function gamma between two pre-ordered sets
-- has to satisfy forall x,y. alpha x ⊑ y iff x ⊑ gamma y
class (PreOrd x p, PreOrd y p) => Galois x y p where
  alpha :: p x y
  gamma :: p y x

instance (Galois x x' p, Galois y y' p) => Galois (x,y) (x',y') p where
  alpha = alpha *** alpha
  gamma = gamma *** gamma


