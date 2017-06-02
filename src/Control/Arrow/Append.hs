{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Append where

import Control.Arrow hiding ((<+>))
import Control.Monad

import Data.Order

class Arrow p => ArrowAppend p where
  (<+>) :: Lattice b => p a b -> p a b -> p a b
  alternatives :: (Functor f, Foldable f) => p (f a) a

instance MonadPlus m => ArrowAppend (Kleisli m) where
  Kleisli f <+> Kleisli g = Kleisli $ \a -> f a `mplus` g a
  alternatives = Kleisli $ \as -> msum (fmap return as)
