{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Arrow.Transformer.Result where

import Prelude hiding (id,(.),fail)

import Result
import Interpreter

import Control.Category
import Control.Arrow hiding ((<+>))
import Control.Arrow.Operations
import Control.Arrow.Transformer.Deduplicate

import Data.Functor.Strength

newtype ResultArrow p a b = ResultArrow { runResult :: p a (Result b)}

instance ArrowChoice p => Category (ResultArrow p) where
  id = ResultArrow (arr Success)
  ResultArrow g . ResultArrow f = ResultArrow $ proc a -> do
    rb <- f -< a
    case rb of
      Success b -> g -< b
      Fail -> returnA -< Fail

instance ArrowChoice p => Arrow (ResultArrow p) where
  arr f = ResultArrow (arr (return . f))
  first (ResultArrow f) = ResultArrow (first f >>> arr strengthL)
  second (ResultArrow f) = ResultArrow (second f >>> arr strengthR)

instance ArrowChoice p => ArrowChoice (ResultArrow p) where
  left (ResultArrow f) = ResultArrow (left f >>> arr costrengthL)
  right (ResultArrow f) = ResultArrow (right f >>> arr costrengthR)

instance (ArrowApply p,ArrowChoice p) => ArrowApply (ResultArrow p) where
  app = ResultArrow $ proc (ResultArrow p,b) -> p -<< b

instance ArrowChoice p => Try (ResultArrow p) where
  fail = ResultArrow (arr (const Fail))
  try (ResultArrow f) (ResultArrow g) (ResultArrow h) = ResultArrow $ proc a -> do
    rb <- f -< a
    case rb of
      Success b -> g -< b
      Fail -> h -< a

instance (ArrowAppend p, ArrowChoice p) => ArrowAppend (ResultArrow p) where
  -- zeroArrow = ResultArrow (arr (const mempty))
  ResultArrow f <+> ResultArrow g = ResultArrow (f <+> g)
    --                                 proc a -> do
    -- (bs,bs') <- f *** g -< (a,a)
    -- returnA -< bs <> bs'

instance (ArrowState s p, ArrowChoice p) => ArrowState s (ResultArrow p) where
  fetch = ResultArrow (fetch >>> arr return)
  store = ResultArrow (store >>> arr return)

instance (ArrowChoice p, Deduplicate p) => Deduplicate (ResultArrow p) where
  dedup (ResultArrow p) = ResultArrow (dedup p)
