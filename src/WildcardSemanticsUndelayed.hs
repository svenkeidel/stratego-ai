{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WildcardSemanticsUndelayed where

import Prelude hiding (fail)

import InterpreterArrow
import Syntax hiding (Fail,TermPattern(..))
import WildcardSemantics 

import Data.UncertainResult

eval :: Int -> StratEnv -> Strat -> (Term,TermEnv) -> UncertainResult (Term,TermEnv)
eval i senv s = runInterp (eval' i s) senv
{-# INLINE eval #-}
