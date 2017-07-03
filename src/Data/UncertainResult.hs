{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
module Data.UncertainResult where

import Control.Arrow
import Control.Monad
import Control.Applicative
import Data.Order

data UncertainResult a
  = Success a
  | Fail
  | SuccessOrFail a

instance Functor UncertainResult where
  fmap f = proc r -> case r of
    Success a -> Success ^<< f -< a
    Fail -> returnA -< Fail
    SuccessOrFail a -> SuccessOrFail ^<< f -< a

instance Applicative UncertainResult where
  pure = return
  (<*>) = ap


instance Monad UncertainResult where
  return = arr Success
  f >>= k = mu (fmap k f)
    where
      mu r = case r of
        Success (Success x) -> Success x
        Success (SuccessOrFail x) -> SuccessOrFail x
        SuccessOrFail (SuccessOrFail x) -> SuccessOrFail x
        SuccessOrFail (Success x) -> SuccessOrFail x
        _ -> Fail

instance Alternative UncertainResult where
  empty = mzero
  (<|>) = mplus 

instance MonadPlus UncertainResult where
  mzero = Fail
  mplus f g = case (f,g) of
    (Success x, Success _) -> Success x
    (Success x, Fail) -> SuccessOrFail x
    (Fail, Success y) -> SuccessOrFail y
    (Fail, Fail) -> Fail
    (SuccessOrFail x, Success _) -> SuccessOrFail x
    (Success x, SuccessOrFail _) -> SuccessOrFail x
    (SuccessOrFail x, Fail) -> SuccessOrFail x
    (Fail, SuccessOrFail y) -> SuccessOrFail y
    (SuccessOrFail x, SuccessOrFail _) -> SuccessOrFail x


instance (PreOrd a p, ArrowChoice p) => PreOrd (UncertainResult a) p where
  (⊑) = proc m -> case m of
    (Fail, Fail) -> returnA -< True
    (Success a, Success b) -> (⊑) -< (a,b)
    (Success a, SuccessOrFail b) -> (⊑) -< (a,b)
    (Fail, SuccessOrFail _) -> returnA -< True
    (_, _) -> returnA -< False

instance (PartOrd a p, ArrowChoice p) => PartOrd (UncertainResult a) p

instance (Lattice a p, ArrowChoice p) => Lattice (UncertainResult a) p where
  (⊔) = proc m -> case m of
    (Success x, Success y) -> Success ^<< (⊔) -< (x,y)
    (Success x, Fail) -> returnA -< SuccessOrFail x
    (Fail, Success y) -> returnA -< SuccessOrFail y
    (Fail, Fail) -> returnA -< Fail
    (SuccessOrFail x, Success y) -> SuccessOrFail ^<< (⊔) -< (x,y)
    (Success x, SuccessOrFail y) -> SuccessOrFail ^<< (⊔) -< (x,y)
    (SuccessOrFail x, Fail) -> returnA -< SuccessOrFail x
    (Fail, SuccessOrFail y) -> returnA -< SuccessOrFail y
    (SuccessOrFail x, SuccessOrFail y) -> SuccessOrFail ^<< (⊔) -< (x,y)

