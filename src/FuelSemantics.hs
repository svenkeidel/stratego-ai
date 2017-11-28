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
import           Control.Arrow.Deduplicate

import           Data.Powerset
import           Data.Result
import           Data.TermEnv
import           Data.Proxy
import           Data.Order
import           Data.Term

newtype Interp a b = Interp (Kleisli (ReaderT (StratEnv,Int) (StateT TermEnv (ResultT Pow))) a b)
  deriving (Category,Arrow,ArrowChoice,ArrowApply,ArrowTry,ArrowZero,ArrowPlus,ArrowDeduplicate,PreOrd,PartOrd,Lattice)

runInterp :: Interp a b -> Int -> StratEnv -> TermEnv -> a -> Pow (Result (b,TermEnv))
runInterp (Interp f) i senv tenv a = runResultT (runStateT (runReaderT (runKleisli f a) (senv,i)) tenv)

eval :: Int -> Strat -> StratEnv -> TermEnv -> Term -> Pow (Result (Term,TermEnv))
eval i s = runInterp (eval' Proxy s) i

liftK :: (a -> _ b) -> Interp a b
liftK f = Interp (Kleisli f)

instance HasStratEnv Interp where
  readStratEnv = liftK (const (fst <$> ask))
  localStratEnv senv (Interp (Kleisli f)) = liftK (local (first (const senv)) . f)

instance IsTerm Term Interp where
  matchTermAgainstConstructor = matchTermAgainstConstructorDefault
  matchTermAgainstString = matchTermAgainstStringDefault
  matchTermAgainstNumber = matchTermAgainstNumberDefault
  matchTermAgainstExplode = matchTermAgainstExplodeDefault
  equal = equalDefault
  mapSubterms = mapSubtermsDefault
  cons = consDefault
  stringLiteral = stringLiteralDefault
  numberLiteral = numberLiteralDefault
  convertFromList = convertFromListDefault

instance IsTermEnv TermEnv Term Interp where
  lookupTermVar = lookupTermVarDefault
  insertTerm = insertTermDefault
  deleteTermVars = deleteTermVarsDefault
  unionTermEnvs = unionTermEnvsDefault

instance ArrowFix Interp Term where
  fixA f z = proc x -> do
    i <- getFuel -< ()
    if i <= 0
    then returnA -< top
    else localFuel (f (fixA f) z) -< (i-1,x)
    where
      getFuel = liftK (const (snd <$> ask))
      localFuel (Interp (Kleisli g)) = liftK $ \(i,a) -> local (second (const i)) (g a)

-- instance HasStack Term Interp where
--   stackPush _ _ f = proc a -> do
--     i <- liftK (const (snd <$> ask)) -< ()
--     if i <= 0
--     then returnA -< top
--     else localFuel f -< (i-1,a)

instance HasTermEnv TermEnv Interp where
  getTermEnv = liftK (const get)
  putTermEnv = liftK (modify . const)

