{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WildcardSemanticsDelayed where

import Prelude hiding (id,fail,concat,sequence,all,zipWith,(.))

import InterpreterArrow
import Syntax
import WildcardSemantics

import Data.Powerset
import Data.PowersetResult
import Data.Result

eval :: Int -> StratEnv -> Strat -> (Term,TermEnv) -> Pow (Result (Term,TermEnv))
eval i senv s te = unPowRes $ runInterp (eval' i s) senv te
{-# INLINE eval #-}
