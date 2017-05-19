{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
module Utils where

import Prelude hiding (id)
import Control.Category
import Control.Arrow
import Data.Monoid

eqLength :: [a] -> [b] -> Bool
eqLength [] [] = True
eqLength (_:as) (_:bs) = eqLength as bs
eqLength _ _ = False

mapA :: (ArrowChoice p) => p a b -> p [a] [b]
mapA f = proc l -> case l of
  (t:ts) -> do
    t' <- f -< t
    ts' <- mapA f -< ts
    returnA -< (t':ts')
  [] -> returnA -< []

zipWithA :: (ArrowChoice p) => p (a,b) c -> p ([a],[b]) [c]
zipWithA f = proc x -> case x of
  (a:as,b:bs) -> do
    c <- f -< (a,b)
    cs <- zipWithA f -< (as,bs)
    returnA -< c:cs
  _ -> returnA -< []

class Arrow p => ArrowTry p where
  fail :: p t a
  try :: Monoid c => p a b -> p b c -> p a c ->  p a c

success :: ArrowTry p => p a a
success = id
{-# INLINE success #-}

class Arrow p => ArrowAppend p where
  (<+>) :: Monoid b => p a b -> p a b -> p a b

instance ArrowTry (Kleisli Maybe) where
  fail = Kleisli $ const Nothing
  try e s f = Kleisli $ \a ->
                case runKleisli e a of
                  Just b -> runKleisli s b
                  Nothing -> runKleisli f a

instance ArrowTry (Kleisli []) where
  fail = Kleisli $ const []
  try e s f = Kleisli $ \a ->
                case runKleisli e a of
                  [] -> runKleisli f a
                  bs -> bs >>= runKleisli s

instance ArrowAppend (Kleisli []) where
  -- zeroArrow = Kleisli $ const mempty
  Kleisli f <+> Kleisli g = Kleisli $ \a -> f a <> g a
