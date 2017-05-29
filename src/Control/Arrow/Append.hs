{-# LANGUAGE Arrows #-}
module Control.Arrow.Append where

import Control.Arrow hiding ((<+>))
import Control.Monad

class Arrow p => ArrowAppend p where
  (<+>) :: Monoid b => p a b -> p a b -> p a b

alternatives :: (ArrowChoice p, ArrowZero p, ArrowAppend p, Monoid a) => p [a] a
alternatives = proc l -> case l of
  [] -> zeroArrow -< ()
  (x:xs) -> (returnA -< x)
        <+> (alternatives -< xs)

instance MonadPlus m => ArrowAppend (Kleisli m) where
  Kleisli f <+> Kleisli g = Kleisli $ \a -> f a `mplus` g a
