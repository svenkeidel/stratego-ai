{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WildcardSemanticsUndelayed where

import Prelude hiding (fail)

import InterpreterArrow
import Syntax hiding (Fail,TermPattern(..))
import SharedSemantics 
import WildcardSemantics 

import Control.Arrow.Apply

import Data.UncertainResult

eval :: StratEnv -> Strat -> (Term,TermEnv) -> UncertainResult (Term,TermEnv)
eval senv s = runInterp (eval' s) senv
{-# INLINE eval #-}
