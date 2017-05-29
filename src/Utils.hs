{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
module Utils where

import Prelude hiding (id)
import Control.Arrow

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
