{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WildcardSemanticsDelayed where

import Prelude hiding (id,fail,concat,sequence,all,zipWith,(.))

import InterpreterArrow
import Syntax
import SharedSemantics
import WildcardSemantics

import Data.Powerset
import Data.PowersetResult
import Data.Result

eval :: Int -> StratEnv -> Strat -> (Term,TermEnv) -> Pow (Result (Term,TermEnv))
eval i senv s te = unPowRes $ runInterp (eval' i s) senv te
{-# INLINE eval #-}

-- {-# SPECIALISE lift :: Interp StratEnv TermEnv PowersetResult [Term] [Term] -> Interp StratEnv TermEnv PowersetResult Term Term #-}
-- {-# SPECIALISE one :: Interp StratEnv TermEnv PowersetResult Term Term -> Interp StratEnv TermEnv PowersetResult [Term] [Term] #-}
-- {-# SPECIALISE some :: Interp StratEnv TermEnv PowersetResult Term Term -> Interp StratEnv TermEnv PowersetResult [Term] [Term] #-}
-- {-# SPECIALISE all :: Interp StratEnv TermEnv PowersetResult Term Term -> Interp StratEnv TermEnv PowersetResult [Term] [Term] #-}
-- {-# SPECIALISE scope :: [TermVar] -> Interp StratEnv TermEnv PowersetResult Term Term -> Interp StratEnv TermEnv PowersetResult Term Term #-}
-- {-# SPECIALISE let_ :: [(StratVar,Strategy)] -> Strat -> (Strat -> Interp StratEnv TermEnv PowersetResult Term Term) -> Interp StratEnv TermEnv PowersetResult Term Term #-}
