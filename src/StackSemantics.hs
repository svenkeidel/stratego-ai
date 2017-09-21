{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module StackSemantics where

import           Prelude hiding (id,fail,concat,sequence,(.),uncurry)

import           WildcardSemantics
import           Syntax (Strat,StratEnv)
import           SharedSemantics
import           Syntax hiding (Fail,TermPattern(..))

import           Control.Category
import           Control.Monad.Reader hiding (fail,sequence)
import           Control.Monad.State hiding (fail,sequence)
import           Control.Monad.Join (JoinT(..))
import qualified Control.Monad.Join as J
import           Control.Monad.Result
import           Control.Monad.Identity
import           Control.Monad.Powerset
import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Try
import           Control.Arrow.Join
import           Control.Arrow.Debug
import           Control.Arrow.Deduplicate

import           Data.Powerset
import           Data.Result
import           Data.Stack
import           Data.TermEnv
import           Data.Proxy

data Stack = Stack

newtype Interp a b = Interp (Kleisli (ReaderT StratEnv (StateT TermEnv (ResultT (PowT (JoinT Stack Identity))))) a b)
  deriving (Category,Arrow,ArrowChoice,ArrowApply,ArrowTry,ArrowJoin,ArrowDeduplicate)

runInterp :: Interp a b -> Stack -> StratEnv -> TermEnv -> a -> (Pow (Result (b,TermEnv)),Stack)
runInterp (Interp f) st senv tenv a = runIdentity (runJoinT (runPowT (runResultT (runStateT (runReaderT (runKleisli f a) senv) tenv))) st)

eval :: Strat -> Stack -> StratEnv -> TermEnv -> Term -> (Pow (Result (Term,TermEnv)),Stack)
eval = runInterp . eval' Proxy

liftK :: (a -> _ b) -> Interp a b
liftK f = Interp (Kleisli f)

instance HasStratEnv Interp where
  readStratEnv = liftK (const ask)
  localStratEnv senv (Interp (Kleisli f)) = liftK (local (const senv) . f)

instance HasStack Term Interp where
  stackPush = undefined

instance HasTermEnv TermEnv Interp where
  getTermEnv = liftK (const get)
  putTermEnv = liftK (modify . const)

instance ArrowDebug Interp where
  debug _ f = f
