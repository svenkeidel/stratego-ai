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
import           Control.Arrow.Fix
import           Control.Arrow.Debug
import           Control.Arrow.Deduplicate

import           Data.Powerset
import           Data.Result
import           Data.Stack
import           Data.TermEnv
import           Data.Proxy
import           Data.Order

import           Debug.Trace
import           Text.Printf

newtype Interp a b = Interp (Kleisli (ReaderT (StratEnv,Int) (StateT TermEnv (ResultT Pow))) a b)
  deriving (Category,Arrow,ArrowChoice,ArrowApply,ArrowTry,ArrowZero,ArrowPlus,ArrowDeduplicate)

runInterp :: Interp a b -> Int -> StratEnv -> TermEnv -> a -> Pow (Result (b,TermEnv))
runInterp (Interp f) i senv tenv a = runResultT (runStateT (runReaderT (runKleisli f a) (senv,i)) tenv)

eval :: Int -> Strat -> StratEnv -> TermEnv -> Term -> Pow (Result (Term,TermEnv))
eval i s = runInterp (eval' Proxy s) i

liftK :: (a -> _ b) -> Interp a b
liftK f = Interp (Kleisli f)

instance HasStratEnv Interp where
  readStratEnv = liftK (const (fst <$> ask))
  localStratEnv senv (Interp (Kleisli f)) = liftK (local (first (const senv)) . f)

getFuel :: Interp () Int
getFuel = liftK (const (snd <$> ask))

localFuel :: Interp a b -> Interp (Int,a) b
localFuel (Interp (Kleisli f)) = liftK $ \(i,a) -> local (second (const i)) (f a)

instance ArrowFix Interp Term where
  fixA f z = proc x -> do
    i <- liftK (const (snd <$> ask)) -< ()
    if i <= 0
    then returnA -< top
    else localFuel _ -< (i-1,x)

instance HasStack Term Interp where
  stackPush _ _ f = proc a -> do
    i <- liftK (const (snd <$> ask)) -< ()
    if i <= 0
    then returnA -< top
    else localFuel f -< (i-1,a)

instance HasTermEnv TermEnv Interp where
  getTermEnv = liftK (const get)
  putTermEnv = liftK (modify . const)

instance ArrowDebug Interp where
  debug str (Interp (Kleisli f)) = liftK $ \a -> ReaderT $ \r -> StateT $ \s -> ResultT $
    let b = trace (printf "CALL: %s(%s)" str (show (a,s))) $ runResultT (runStateT (runReaderT (f a) r) s)
        bFiltered = filterSuccess b
    in trace (printf "RETURN: %s(%s) -> %s" str (show (a,s)) (show bFiltered)) b
    where
      filterSuccess :: Pow (Result a) -> Pow a
      filterSuccess = foldMap $ \r -> case r of
        Success a -> return a
        Fail -> mempty

