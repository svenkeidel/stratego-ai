{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module SharedSemantics where

import           Prelude hiding (fail,(.),id,sum,flip)

import           Syntax hiding (Fail,TermPattern(..))
import           Utils

import           Control.Arrow hiding (ArrowZero(..),ArrowPlus(..))
import           Control.Arrow.Append
import           Control.Arrow.Try
import           Control.Category

import qualified Data.HashMap.Lazy as M
import           Data.Order
import           Data.Term
import           Data.TermEnv

import           Text.Printf

-- Language Constructs

guardedChoice :: (ArrowTry p, Lattice z) => p x y -> p y z -> p x z -> p x z
guardedChoice = try
{-# INLINE guardedChoice #-}

sequence :: Category p => p a b -> p b c -> p a c
sequence f g = f >>> g
{-# INLINE sequence #-}

lift :: (ArrowChoice p, ArrowTry p, ArrowAppend p, HasTerm t p, Lattice t) => p [t] [t] -> p t t
lift p = proc t -> do
  m <- matchTerm -< t
  case m of
    Cons c ts -> do
      ts' <- p -< ts
      cons -< (c,ts')
    StringLiteral {} -> returnA -< t
    NumberLiteral {} -> returnA -< t
    Wildcard -> fail <+> wildcard -< ()
    _ -> fail -< ()
{-# INLINE lift #-}

one :: (ArrowTry p, ArrowAppend p, ArrowChoice p, Lattice t, Lattice [t]) => p t t -> p [t] [t]
one f = proc l -> case l of
  (t:ts) -> do
    (t',ts') <- first f <+> second (one f) -< (t,ts)
    returnA -< (t':ts')
  [] -> fail -< ()
{-# INLINE one #-}

some :: (ArrowTry p, ArrowChoice p, Lattice t, Lattice [t]) => p t t -> p [t] [t]
some f = go
  where
    go = proc l -> case l of
      (t:ts) -> do
        (t',ts') <- try (first f) (second go') (second go) -< (t,ts)
        returnA -< t':ts'
      -- the strategy did not succeed for any of the subterms, i.e. some(s) failes
      [] -> fail -< ()
    go' = proc l -> case l of
      (t:ts) -> do
        (t',ts') <- try (first f) (second go') (second go') -< (t,ts)
        returnA -< t':ts'
      [] -> returnA -< []
{-# INLINE some #-}

all :: ArrowChoice p => p a b -> p [a] [b]
all = mapA
{-# INLINE all #-}

scope :: (Arrow p, HasTermEnv t env p) => [TermVar] -> p a b -> p a b
scope vars s = proc t -> do
  env  <- getTermEnv     -< ()
  _    <- deleteTermVars -< vars
  t'   <- s              -< t
  _    <- deleteTermVars -< vars
  env' <- getTermEnv     -< ()
  putTermEnv <<< unionTermEnvs -< (env,env')
  returnA -< t'
{-# INLINE scope #-}

let_ :: HasStratEnv p => [(StratVar,Strategy)] -> Strat -> (Strat -> p a b) -> p a b
let_ ss body interp = proc a -> do
  let ss' = [ (v,Closure s' M.empty) | (v,s') <- ss ]
  senv <- readStratEnv -< ()
  localStratEnv (interp body) -< (a,M.union (M.fromList ss') senv) 
{-# INLINE let_ #-}

call :: (ArrowTry p, ArrowChoice p, ArrowApply p, HasTermEnv t env p, HasStratEnv p)
     => StratVar
     -> [Strat]
     -> [TermVar]
     -> (Strat -> p a b)
     -> p a b
call f actualStratArgs actualTermArgs interp = proc a -> do
  senv <- readStratEnv -< ()
  case M.lookup f senv of
    Just (Closure (Strategy formalStratArgs formalTermArgs body) senv') -> do
      tenv <- getTermEnv -< ()
      mapA bindTermArg -< zip actualTermArgs formalTermArgs
      let senv'' = bindStratArgs (zip formalStratArgs actualStratArgs)
                                 (if M.null senv' then senv else senv')
      b <- localStratEnv (interp body) -<< (a,senv'')
      deleteTermVars -< formalTermArgs
      tenv' <- getTermEnv -< ()
      putTermEnv <<< unionTermEnvs -< (tenv,tenv')
      returnA -< b
    Nothing -> error (printf "strategy %s not in scope" (show f)) -< ()
  where
    bindTermArg = proc (actual,formal) -> do
     m <- lookupTermVar -< actual
     case m of
       Just t -> insertTerm -< (formal,t)
       Nothing -> fail -< ()
     returnA -< ()
    {-# INLINE bindTermArg #-}

bindStratArgs :: [(StratVar,Strat)] -> StratEnv -> StratEnv
bindStratArgs [] senv = senv
bindStratArgs ((v,Call v' [] []) : ss) senv =
  case M.lookup v' senv of
    Just s -> M.insert v s (bindStratArgs ss senv)
    _ -> error $ "unknown strategy: " ++ show v'
bindStratArgs ((v,s) : ss) senv =
    M.insert v (Closure (Strategy [] [] s) senv) (bindStratArgs ss senv)

convertToList :: (ArrowChoice p, HasTerm t p) => p [t] t
convertToList = proc ts -> case ts of
  (x:xs) -> do
    l <- convertToList -< xs
    cons -< ("Cons",[x,l])
  [] -> cons -< ("Nil",[])
    
convertFromList :: (ArrowChoice p, ArrowTry p, HasTerm t p) => p t (Maybe [t])
convertFromList = proc t -> do
  m <- matchTerm -< t
  case m of
    Cons "Cons" [x,tl] -> do
      xs <- convertFromList -< tl
      returnA -< (x:) <$> xs
    Cons "Nil" [] ->
      returnA -< Just []
    Wildcard -> returnA -< Nothing
    _ -> fail -< ()
