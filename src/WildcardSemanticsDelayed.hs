module WildcardSemanticsDelayed where

import Prelude hiding (id,fail,concat,sequence,all,zipWith,(.))

import InterpreterArrow
import Syntax
import SharedSemantics
import WildcardSemantics

import Data.Powerset
import Data.PowersetResult
import Data.Result

eval'' :: Strat -> Interp StratEnv TermEnv Stack PowersetResult Term Term
eval'' = eval' 

eval :: StratEnv -> Strat -> (Term,(TermEnv,Stack)) -> Pow (Result (Term,(TermEnv,Stack)))
eval senv s te = unPowRes $ runInterp (eval' s) senv te
-- eval n m senv s te = unPowRes $ runInterp (eval' n m $$ s) senv te
{-# INLINE eval #-}

