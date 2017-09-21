{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.Stack where

import Syntax
import Data.Order

type CallSignature = (Strategy,[Strat],[TermVar])

class HasStack t c | c -> t where
  stackPush :: (BoundedLattice t) => StratVar -> CallSignature -> c t t -> c t t

