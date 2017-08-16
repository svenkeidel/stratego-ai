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
import           Stack

import           Control.Arrow hiding (ArrowZero(..),ArrowPlus(..))
import           Control.Arrow.Apply
import           Control.Arrow.Join
import           Control.Arrow.Try
import           Control.Category

import qualified Data.HashMap.Lazy as M
import           Data.Order
import           Data.Complete
import           Data.Term
import           Data.TermEnv
import           Data.Hashable
import           Data.Powerset (Deduplicate(..))

import           Text.Printf
import           Debug.Trace

-- Language Constructs
eval' :: (ArrowChoice c, ArrowTry c, ArrowJoin c, ArrowApply c, Deduplicate c,
          HasStratEnv c, Eq t, Hashable t, IsTerm t c, BoundedLattice t c, IsTermEnv env t c,
          HasAlloc addr c, HasStack ts addr t c)
      => Strat -> c t t
eval' s0 = dedup $ case s0 of
    Id -> id
    S.Fail -> fail
    Seq s1 s2 -> (eval' s2) . (eval' s1)
    GuardedChoice s1 s2 s3 -> try (eval' s1) (eval' s2) (eval' s3)
    One s -> lift (one (eval' s))
    Some s -> lift (some (eval' s))
    All s -> lift (all (eval' s))
    Scope xs s -> scope xs (eval' s)
    Match f -> proc t -> match -< (f,t)
    Build f -> proc _ -> build -< f
    Let bnds body -> let_ bnds body eval'
    Call f ss ps -> call f ss ps eval'
    Prim f _ ps -> prim f ps
                  
-- eval' n m = fixA' n m $ \(evalD,evalS) -> dedup $ uncurry $ arr $ \s0 -> case s0 of
--     Id -> id
--     S.Fail -> fail
--     Seq s1 s2 -> (evalS $$ s2) . (evalS $$ s1)
--     GuardedChoice s1 s2 s3 -> try (evalS $$ s1) (evalS $$ s2) (evalS $$ s3)
--     One s -> lift (one (evalS $$ s))
--     Some s -> lift (some (evalS $$ s))
--     All s -> lift (all (evalS $$ s))
--     Scope xs s -> scope xs (evalS $$ s)
--     Match f -> proc t -> match -< (f,t)
--     Build f -> proc _ -> build -< f
--     Let bnds body -> let_ bnds body evalS
--     Call f ss ps -> call f ss ps evalD
--     Prim f _ ps -> prim f ps

prim :: (ArrowTry p, IsTerm t p, IsTermEnv env t p) => StratVar -> [TermVar] -> p t t
prim = undefined

guardedChoice :: (ArrowTry c, Lattice (Complete z) c) => c x y -> c y z -> c x z -> c x z
guardedChoice = try
{-# INLINE guardedChoice #-}

sequence :: Category c => c x y -> c y z -> c x z
sequence f g = f >>> g
{-# INLINE sequence #-}

one :: (ArrowTry c, ArrowJoin c, ArrowChoice c, PartOrd t c, Lattice (Complete t) c) => c t t -> c [t] [t]
one f = proc l -> case l of
  (t:ts) -> do
    (t',ts') <- first f <+> second (one f) -< (t,ts)
    returnA -< (t':ts')
  [] -> fail -< ()
{-# INLINE one #-}

some :: (ArrowTry c, ArrowChoice c, PartOrd t c, Lattice (Complete t) c) => c t t -> c [t] [t]
some f = go
  where
    go = proc l -> case l of
      (t:ts) -> do
        (t',ts') <- try (first f) (second go') (second go) -< (t,ts)
        returnA -< t':ts'
      -- the strategy did not succeed for any of the subterms, i.e. some(s) fails
      [] -> fail -< ()
    go' = proc l -> case l of
      (t:ts) -> do
        (t',ts') <- try (first f) (second go') (second go') -< (t,ts)
        returnA -< t':ts'
      [] -> returnA -< []
{-# INLINE some #-}

all :: ArrowChoice c => c x y -> c [x] [y]
all = mapA
{-# INLINE all #-}

scope :: (Arrow c, IsTermEnv env t c) => [TermVar] -> c x y -> c x y
scope vars s = proc t -> do
  env  <- getTermEnv     -< ()
  _    <- deleteTermVars -< vars
  t'   <- s              -< t
  env' <- getTermEnv     -< ()
  putTermEnv <<< unionTermEnvs -< (vars,env,env')
  returnA -< t'
{-# INLINE scope #-}

let_ :: (ArrowApply c, HasStratEnv c) => [(StratVar,Strategy)] -> Strat -> (Strat -> c t t) -> c t t
let_ ss body interp = proc a -> do
  let ss' = [ (v,Closure s' M.empty) | (v,s') <- ss ]
  senv <- readStratEnv -< ()
  localStratEnv (interp body) -< (a,M.union (M.fromList ss') senv) 
{-# INLINE let_ #-}

call :: (ArrowTry c, ArrowChoice c, ArrowApply c, IsTermEnv env t c, HasStratEnv c, HasAlloc addr c, HasStack ts addr t c)
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
      addr <- alloc -< (strat, actualStratArgs, actualTermArgs)
      b <- localStratEnv (localStackPush addr (interp body)) -<< (a,senv'')
      tenv' <- getTermEnv -< ()
      putTermEnv <<< unionTermEnvs -< (formalTermArgs,tenv,tenv')
      returnA -< b
    Nothing -> error (printf "strategy %s not in scope" (show f)) -< ()
  where
    bindTermArg = proc (actual,formal) ->
      lookupTermVar (proc t -> insertTerm -< (formal,t)) fail -<< actual
    {-# INLINE bindTermArg #-}

bindStratArgs :: [(StratVar,Strat)] -> StratEnv -> StratEnv
bindStratArgs [] senv = senv
bindStratArgs ((v,Call v' [] []) : ss) senv =
  case M.lookup v' senv of
    Just s -> M.insert v s (bindStratArgs ss senv)
    _ -> error $ "unknown strategy: " ++ show v'
bindStratArgs ((v,s) : ss) senv =
    M.insert v (Closure (Strategy [] [] s) senv) (bindStratArgs ss senv)
 
match :: (ArrowChoice c, ArrowJoin c, ArrowTry c, ArrowApply c,
          IsTerm t c, IsTermEnv env t c)
      => c (TermPattern,t) t
match = proc (p,t) -> case p of
  S.As v p2 -> do
    t' <- match -< (S.Var v,t)
    match -< (p2,t')
  S.Var "_" ->
    success -< t
  S.Var x ->
    lookupTermVar
      (proc t' -> equal -< (t,t'))
      (proc () -> do insertTerm -< (x,t); returnA -< t) -<< x
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

build :: (ArrowChoice c, ArrowJoin c, ArrowTry c, IsTerm t c, IsTermEnv env t c)
      => c TermPattern t
build = proc p -> case p of
  S.As _ _ -> error "As-pattern in build is disallowed" -< ()
  S.Var x ->
    lookupTermVar returnA fail -< x
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
