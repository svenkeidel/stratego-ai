module Data.PowersetResult where

import           Control.Monad

import           Data.Hashable
import           Data.Powerset
import qualified Data.Powerset as P
import           Data.Result

newtype PowersetResult a = PowRes {unPowRes :: Pow (Result a)}

instance Functor PowersetResult where
  fmap f (PowRes p) = PowRes (fmap (fmap f) p)

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

empty :: PowersetResult a
empty = PowRes mempty

union :: PowersetResult a -> PowersetResult a -> PowersetResult a
union (PowRes a) (PowRes b) = PowRes (a `P.union` b)
{-# INLINE union #-}
                             
dedup' :: (Eq a, Hashable a) => PowersetResult a -> PowersetResult a
dedup' (PowRes a) = PowRes (P.dedup' a)
