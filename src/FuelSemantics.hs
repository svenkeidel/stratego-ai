{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module FuelSemantics where

import           Prelude hiding (id,fail,concat,sequence,(.),uncurry)

import           WildcardSemantics
import           Syntax (Strat,StratEnv)
import           SharedSemantics
import           Syntax hiding (Fail,TermPattern(..))

import           Control.Category
import           Control.Monad.Reader hiding (fail,sequence)
import           Control.Monad.State hiding (fail,sequence)
import           Control.Monad.Result
import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Try
import           Control.Arrow.Join
import           Control.Arrow.Deduplicate

import           Data.Powerset
import           Data.Result
import           Data.Stack
import           Data.TermEnv
import           Data.Proxy
import           Data.Order

data Stack = Stack

newtype Interp a b = Interp (Kleisli (ReaderT (StratEnv,Int) (StateT TermEnv (ResultT Pow))) a b)
  deriving (Category,Arrow,ArrowChoice,ArrowApply,ArrowTry,ArrowJoin,ArrowDeduplicate)

runInterp :: Interp a b -> Int -> StratEnv -> TermEnv -> a -> Pow (Result (b,TermEnv))
runInterp (Interp f) i senv tenv a = runResultT (runStateT (runReaderT (runKleisli f a) (senv,i)) tenv)

eval :: Strat -> Int -> StratEnv -> TermEnv -> Term -> Pow (Result (Term,TermEnv))
eval = runInterp . eval' Proxy

liftK :: (a -> _ b) -> Interp a b
liftK f = Interp (Kleisli f)

instance HasStratEnv Interp where
  readStratEnv = liftK (const (fst <$> ask))
  localStratEnv senv (Interp (Kleisli f)) = liftK (local (first (const senv)) . f)

getFuel :: Interp () Int
getFuel = liftK (const (snd <$> ask))

localFuel :: Interp a b -> Interp (Int,a) b
localFuel (Interp (Kleisli f)) = liftK $ \(i,a) -> local (second (const i)) (f a)

instance HasStack Interp where
  stackPush _ f = proc a -> do
    i <- liftK (const (snd <$> ask)) -< ()
    if i <= 0
    then returnA -< top
    else localFuel f -< (i-1,a)

instance HasTermEnv TermEnv Interp where
  getTermEnv = liftK (const get)
  putTermEnv = liftK (modify . const)
