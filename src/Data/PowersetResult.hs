{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.PowersetResult where

import           Prelude hiding (map,(.),id)

import           Control.Monad
import           Control.Arrow
import           Control.Category

import           Data.Hashable
import           Data.Powerset (Pow)
import qualified Data.Powerset as P
import           Data.Result (Result(..))
import qualified Data.Result as R
import           Data.Order
import           Data.Foldable (toList)

newtype PowersetResult a = PowRes { unPowRes :: Pow (Result a) }
  deriving (Functor,Monoid)

map :: ArrowChoice c => c x y -> c (PowersetResult x) (PowersetResult y)
map f = proc (PowRes a) -> PowRes ^<< P.map (R.map f) -< a

collect :: Result (Pow a) -> Pow (Result a)
collect r = case r of
  Success x -> Success <$> x
  Fail -> return Fail

instance Applicative PowersetResult where
  pure = return
  (<*>) = ap

instance Monad PowersetResult where
  return = PowRes . return . return
  PowRes p >>= k = PowRes $ do
    r <- p
    case r of
      Success a -> unPowRes (k a)
      Fail -> return Fail

instance Show a => Show (PowersetResult a) where
  show (PowRes a) = show a

empty :: PowersetResult a
empty = PowRes mempty

union :: PowersetResult a -> PowersetResult a -> PowersetResult a
union (PowRes a) (PowRes b) = PowRes (a `P.union` b)
{-# INLINE union #-}

fromFoldable :: Foldable f => f (Result a) -> PowersetResult a
fromFoldable = PowRes . P.fromFoldable
{-# INLINE fromFoldable #-}

dedup' :: (Eq a, Hashable a) => PowersetResult a -> PowersetResult a
dedup' (PowRes a) = PowRes (P.dedup' a)
{-# INLINE dedup' #-}

instance PreOrd a => PreOrd (PowersetResult a) where
  PowRes xs ⊑ PowRes ys = all (\x -> any (\y -> x ⊑ y) (toList ys)) (toList xs)

instance (Galois (Pow x) x', Eq x, Hashable x)
  => Galois (Pow (Result x)) (PowersetResult x') where
  alpha xs = PowRes (fmap (fmap (\x -> alpha (P.singleton x))) xs)
  gamma = undefined -- arr (join . fmap collect . unPowRes) . map (gamma :: p x' (Pow x))
