{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module SharedSemantics where

import           Prelude hiding (fail,(.),id,sum,flip,uncurry,all)

import           Syntax hiding (Fail,TermPattern(..))
import           Syntax (TermPattern)
import qualified Syntax as S
import           Utils

import           Control.Arrow hiding (ArrowZero(..),ArrowPlus(..))
import           Control.Arrow.Join
import           Control.Arrow.Try
import           Control.Arrow.Deduplicate
import           Control.Arrow.Debug
import           Control.Category

import qualified Data.HashMap.Lazy as M
import           Data.Order
import           Data.Complete
import           Data.Term
import           Data.TermEnv
import           Data.Hashable
import           Data.Stack
import           Data.Proxy

import           Text.Printf

-- Language Constructs
eval' :: (ArrowChoice c, ArrowTry c, ArrowJoin c, ArrowApply c, ArrowDeduplicate c,
          ArrowDebug c, Show t,
          BoundedLattice t, Eq t, Hashable t, 
          HasStratEnv c, HasStack t c, IsTerm t, HasTermEnv env c, IsTermEnv env t)
      => Proxy env -> Strat -> c t t
eval' p s0 = dedupA $ case s0 of
    Id -> id
    S.Fail -> failA
    Seq s1 s2 -> eval' p s2 . eval' p s1
    GuardedChoice s1 s2 s3 -> tryA (eval' p s1) (eval' p s2) (eval' p s3)
    One s -> lift (one (eval' p s))
    Some s -> lift (some (eval' p s))
    All s -> lift (all (eval' p s))
    Scope xs s -> scope xs (eval' p s)
    Match f -> proc t -> match -< (f,t)
    Build f -> proc _ -> build -< f
    Let bnds body -> let_ bnds body (eval' p)
    Call f ss ps -> call f ss ps (eval' p)
    Prim f _ ps -> prim f ps
                  
prim :: (ArrowTry p) => StratVar -> [TermVar] -> p t t
prim = undefined

guardedChoice :: (ArrowTry c, Lattice (Complete z)) => c x y -> c y z -> c x z -> c x z
guardedChoice = tryA
{-# INLINE guardedChoice #-}

sequence :: Category c => c x y -> c y z -> c x z
sequence f g = f >>> g
{-# INLINE sequence #-}

one :: Ar c => c t t -> c [t] [t]
one f = proc l -> case l of
  (t:ts) -> do
    (t',ts') <- first f <+> second (one f) -< (t,ts)
    returnA -< (t':ts')
  [] -> failA -< ()
{-# INLINE one #-}

some :: Ar c => c t t -> c [t] [t]
some f = go
  where
    go = proc l -> case l of
      (t:ts) -> do
        (t',ts') <- tryA (first f) (second go') (second go) -< (t,ts)
        returnA -< t':ts'
      -- the strategy did not succeed for any of the subterms, i.e. some(s) fails
      [] -> failA -< ()
    go' = proc l -> case l of
      (t:ts) -> do
        (t',ts') <- tryA (first f) (second go') (second go') -< (t,ts)
        returnA -< t':ts'
      [] -> returnA -< []
{-# INLINE some #-}

all :: ArrowChoice c => c x y -> c [x] [y]
all = mapA
{-# INLINE all #-}

scope :: (Ar c, HasTermEnv env c, IsTermEnv env t) => [TermVar] -> c x y -> c x y
scope vars s = proc t -> do
  env  <- getTermEnv      -< ()
  _    <- deleteTermVars' -< vars
  t'   <- s               -< t
  env' <- getTermEnv      -< ()
  putTermEnv <<< unionTermEnvs -< (vars,env,env')
  returnA -< t'
{-# INLINE scope #-}

let_ :: (ArrowApply c, HasStratEnv c) => [(StratVar,Strategy)] -> Strat -> (Strat -> c t t) -> c t t
let_ ss body interp = proc a -> do
  let ss' = [ (v,Closure s' M.empty) | (v,s') <- ss ]
  senv <- readStratEnv -< ()
  localStratEnv (M.union (M.fromList ss') senv) (interp body) -<< a 
{-# INLINE let_ #-}

call :: (Ar c, ArrowApply c, ArrowDebug c,
         HasTermEnv env c, IsTermEnv env t,
         HasStratEnv c, HasStack t c,
         Show t, BoundedLattice t)
     => StratVar
     -> [Strat]
     -> [TermVar]
     -> (Strat -> c t t)
     -> c t t
call f actualStratArgs actualTermArgs interp = proc a -> do
  senv <- readStratEnv -< ()
  case M.lookup f senv of
    Just (Closure strat@(Strategy formalStratArgs formalTermArgs body) senv') -> do
      tenv <- getTermEnv -< ()
      mapA bindTermArg -< zip actualTermArgs formalTermArgs
      let senv'' = bindStratArgs (zip formalStratArgs actualStratArgs)
                                 (if M.null senv' then senv else senv')
          callSignature = (strat,actualStratArgs,actualTermArgs)
      b <- localStratEnv senv'' (stackPush f callSignature (interp body)) -<< a
      tenv' <- getTermEnv -< ()
      putTermEnv <<< unionTermEnvs -< (formalTermArgs,tenv,tenv')
      returnA -< b
    Nothing -> error (printf "strategy %s not in scope" (show f)) -< ()
  where
    bindTermArg = proc (actual,formal) ->
      lookupTermVar' (proc t -> insertTerm' -< (formal,t)) failA -<< actual
    {-# INLINE bindTermArg #-}

bindStratArgs :: [(StratVar,Strat)] -> StratEnv -> StratEnv
bindStratArgs [] senv = senv
bindStratArgs ((v,Call v' [] []) : ss) senv =
  case M.lookup v' senv of
    Just s -> M.insert v s (bindStratArgs ss senv)
    _ -> error $ "unknown strategy: " ++ show v'
bindStratArgs ((v,s) : ss) senv =
    M.insert v (Closure (Strategy [] [] s) senv) (bindStratArgs ss senv)
 
match :: (Ar c, ArrowApply c, IsTerm t, HasTermEnv env c, IsTermEnv env t)
      => c (TermPattern,t) t
match = proc (p,t) -> case p of
  S.As v p2 -> do
    t' <- match -< (S.Var v,t)
    match -< (p2,t')
  S.Var "_" ->
    success -< t
  S.Var x ->
    lookupTermVar'
      (proc t' -> do t'' <- equal -< (t,t'); insertTerm' -< (x,t''); returnA -< t'')
      (proc () -> do insertTerm' -< (x,t); returnA -< t) -<< x
  S.Cons c ts ->
    matchTermAgainstConstructor (zipWithA match) -< (c,ts,t)
  S.Explode c ts ->
    matchTermAgainstExplode
      (proc c' ->  match -< (c,c'))
      (proc ts' -> match -< (ts,ts')) -<< t
  S.StringLiteral s ->
    matchTermAgainstString -< (s,t)
  S.NumberLiteral n ->
    matchTermAgainstNumber -< (n,t)

build :: (Ar c, IsTerm t, HasTermEnv env c, IsTermEnv env t)
      => c TermPattern t
build = proc p -> case p of
  S.As _ _ -> error "As-pattern in build is disallowed" -< ()
  S.Var x ->
    lookupTermVar' returnA failA -< x
  S.Cons c ts -> do
    ts' <- mapA build -< ts
    cons -< (c,ts')
  S.Explode c ts -> do
    c'  <- build -< c
    ts' <- build -< ts
    convertFromList -< (c',ts')
  S.NumberLiteral n ->
    numberLiteral -< n
  S.StringLiteral s ->
    stringLiteral -< s
