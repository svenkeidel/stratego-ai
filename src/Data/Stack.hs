module Data.Stack where

import Syntax
import Data.Order

type CallSignature = (Strategy,[Strat],[TermVar])

class HasStack c where
  stackPush :: BoundedLattice t => CallSignature -> c t t -> c t t

