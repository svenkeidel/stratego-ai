{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Arrow.Append where

import Control.Arrow hiding ((<+>))
import Control.Monad

import Data.Order
import Data.Complete

class Arrow c => ArrowAppend c where
  (<+>) :: Lattice (Complete y) c => c x y -> c x y -> c x y
  alternatives :: (Functor f, Foldable f) => c (f x) x

instance MonadPlus m => ArrowAppend (Kleisli m) where
  Kleisli f <+> Kleisli g = Kleisli $ \a -> f a `mplus` g a
  alternatives = Kleisli $ \as -> msum (fmap return as)
