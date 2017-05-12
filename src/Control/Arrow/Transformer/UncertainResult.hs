{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.UncertainResult where

import Prelude hiding (id,(.),fail)

import UncertainResult
import Interpreter

import Control.Category
import Control.Arrow
import Control.Arrow.Operations
 
import Data.Functor.Strength

newtype UncertainResultArrow p a b = UncertainResultArrow { runUncertainResult :: p a (UncertainResult b) }

instance ArrowChoice p => Category (UncertainResultArrow p) where
  id = UncertainResultArrow (arr Success)
  UncertainResultArrow g . UncertainResultArrow f = UncertainResultArrow $ proc a -> do
    rb <- f -< a
    case rb of
      Success b -> g -< b
      SuccessOrFail b -> do
        rc <- g -< b
        returnA -< successOrFail rc
      Fail -> returnA -< Fail

instance ArrowChoice p => Arrow (UncertainResultArrow p) where
  arr f = UncertainResultArrow (arr (return . f))
  first (UncertainResultArrow f) = UncertainResultArrow (first f >>> arr strengthL)
  second (UncertainResultArrow f) = UncertainResultArrow (second f >>> arr strengthR)

instance ArrowChoice p => ArrowChoice (UncertainResultArrow p) where
  left (UncertainResultArrow f) = UncertainResultArrow (left f >>> arr costrengthL)
  right (UncertainResultArrow f) = UncertainResultArrow (right f >>> arr costrengthR)

instance (ArrowApply p,ArrowChoice p) => ArrowApply (UncertainResultArrow p) where
  app = UncertainResultArrow $ proc (UncertainResultArrow p,b) -> p -<< b

instance ArrowChoice p => ArrowTry (UncertainResultArrow p) where
  fail = UncertainResultArrow (arr (const Fail))
  try (UncertainResultArrow f) (UncertainResultArrow g) (UncertainResultArrow h) = UncertainResultArrow $ proc a -> do
    rb <- f -< a
    case rb of
      Success b -> g -< b
      SuccessOrFail b -> do
        (rc,rc') <- g *** h -< (b,a)
        returnA -< rc `mappend` rc'
      Fail -> h -< a

instance ArrowChoice p => ArrowAppend (UncertainResultArrow p) where
  -- zeroArrow = UncertainResultArrow (arr (const mempty))
  UncertainResultArrow f <+> UncertainResultArrow g = UncertainResultArrow $ proc a -> do
    (bs,bs') <- f *** g -< (a,a)
    returnA -< bs `mappend` bs'

instance (ArrowState s p, ArrowChoice p) => ArrowState s (UncertainResultArrow p) where
  fetch = UncertainResultArrow (fetch >>> arr return)
  store = UncertainResultArrow (store >>> arr return)
