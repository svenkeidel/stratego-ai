{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
module Example where

import Prelude hiding (mapM)
import Control.Arrow

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f l = case l of
  (a:as) -> do
     b <- f a
     bs <- mapM f as
     return (b:bs)
  [] -> return []

mapA :: ArrowChoice p => p a b -> p [a] [b]
mapA f = proc l -> case l of
  (a:as) -> do
    b <- f -< a
    bs <- mapA f -< as
    returnA -< b:bs
  [] -> returnA -< []
