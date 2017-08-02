module WildcardSemanticsDelayed where

import Prelude hiding (id,fail,concat,sequence,all,zipWith,(.))

import InterpreterArrow
import Syntax
import SharedSemantics
import WildcardSemantics

import Data.Powerset
import Data.PowersetResult
import Data.Result

eval'' :: Int -> Strat -> Interp StratEnv TermEnv PowersetResult Term Term
eval'' = eval' 

eval :: Int -> StratEnv -> Strat -> (Term,TermEnv) -> Pow (Result (Term,TermEnv))
eval n senv s te = unPowRes $ runInterp (eval' n s) senv te
-- eval n m senv s te = unPowRes $ runInterp (eval' n m $$ s) senv te
{-# INLINE eval #-}
