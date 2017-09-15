module Data.Stack where

import Syntax

import Data.Order

type CallSignature = (Strategy,[Strat],[TermVar])

data Stack = Stack

instance PreOrd Stack where
  (⊑) = undefined

instance PartOrd Stack 

instance Lattice Stack where
  (⊔) = undefined

class HasStack c where
  stackPush :: CallSignature -> c t t -> c t t

