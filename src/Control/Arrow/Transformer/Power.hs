{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Power where

import           Prelude hiding (id,(.),fail)

import           Interpreter

import           Control.Category
import           Control.Arrow hiding (ArrowZero(..),ArrowPlus(..))
import           Control.Arrow.Operations
import           Control.Arrow.Transformer.Deduplicate
import           Control.Monad hiding (fail)

import           Data.Functor.Strength
import           Data.Sequence 
import           Data.Semigroup

type Pow = Seq

newtype PowerArrow p a b = PowerArrow { runPower :: p a (Pow b) }

instance ArrowChoice p => Category (PowerArrow p) where
  id = PowerArrow (arr return)
  PowerArrow p . PowerArrow q = PowerArrow $ proc a -> do
    bs <- q -< a
    cs <- mapPow p -< bs
    returnA -< join cs

instance ArrowChoice p => Arrow (PowerArrow p) where
  arr f = PowerArrow (arr (return . f))
  first (PowerArrow f) = PowerArrow (first f >>> arr strengthL)
  second (PowerArrow f) = PowerArrow (second f >>> arr strengthR)

instance ArrowChoice p => ArrowChoice (PowerArrow p) where
  left (PowerArrow f) = PowerArrow (left f >>> arr costrengthL)
  right (PowerArrow f) = PowerArrow (right f >>> arr costrengthR)

instance (ArrowApply p,ArrowChoice p) => ArrowApply (PowerArrow p) where
  app = PowerArrow $ proc (PowerArrow p,b) -> p -<< b

instance (Try p,ArrowChoice p) => Try (PowerArrow p) where
  fail = PowerArrow fail
  try (PowerArrow f) (PowerArrow g) (PowerArrow h) = PowerArrow (try f (mapPow g >>> arr join) h)

instance ArrowChoice p => ArrowAlternative (PowerArrow p) where
  zeroArrow = PowerArrow (arr (const mempty))
  PowerArrow f <+> PowerArrow g = PowerArrow $ proc a -> do
    (bs,bs') <- f *** g -< (a,a)
    returnA -< bs <> bs'

instance (ArrowState s p,ArrowChoice p) => ArrowState s (PowerArrow p) where
  fetch = PowerArrow (fetch >>> arr return)
  store = PowerArrow (store >>> arr return)

mapPow :: ArrowChoice p => p a b -> p (Pow a) (Pow b)
mapPow f = proc p -> case viewl p of
  a :< as -> do
    b <- f -< a
    bs <- mapPow f -< as
    returnA -< (b <| bs)
  EmptyL ->
    returnA -< empty

instance ArrowChoice p => Deduplicate (PowerArrow p) where
  dedup (PowerArrow f) = PowerArrow (f >>> arr dedup')

union = (><)
