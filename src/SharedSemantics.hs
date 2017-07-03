{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
module SharedSemantics where

import           Prelude hiding (fail,(.),id,sum,flip)

import           Syntax hiding (Fail,TermPattern(..))
import           Syntax (TermPattern)
import qualified Syntax as S
import           Utils

import           Control.Arrow hiding (ArrowZero(..),ArrowPlus(..))
import           Control.Arrow.Append
import           Control.Arrow.Try
import           Control.Category

import qualified Data.HashMap.Lazy as M
import           Data.Order
import           Data.Complete
import           Data.Constructor
import           Data.Term
import qualified Data.Term as T
import           Data.TermEnv

import           Text.Printf

-- Language Constructs

guardedChoice :: (ArrowTry c, Lattice (Complete z) c) => c x y -> c y z -> c x z -> c x z
guardedChoice = try
{-# INLINE guardedChoice #-}

sequence :: Category c => c x y -> c y z -> c x z
sequence f g = f >>> g
{-# INLINE sequence #-}

lift :: (ArrowChoice c, ArrowTry c, ArrowAppend c, HasTerm t c, Lattice (Complete t) c) => c [t] [t] -> c t t
lift p = proc t -> do
  m <- matchTermRefine -< t
  case m of
    Cons c ts -> do
      ts' <- p -< ts
      cons -< (c,ts')
    StringLiteral {} -> returnA -< t
    NumberLiteral {} -> returnA -< t
    Wildcard -> fail <+> wildcard -< ()
    _ -> fail -< ()
{-# INLINE lift #-}

one :: (ArrowTry c, ArrowAppend c, ArrowChoice c, PartOrd t c, Lattice (Complete t) c) => c t t -> c [t] [t]
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

scope :: (Arrow c, HasTermEnv t env c) => [TermVar] -> c x y -> c x y
scope vars s = proc t -> do
  env  <- getTermEnv     -< ()
  _    <- deleteTermVars -< vars
  t'   <- s              -< t
  env' <- getTermEnv     -< ()
  putTermEnv <<< unionTermEnvs -< (vars,env,env')
  returnA -< t'
{-# INLINE scope #-}

let_ :: HasStratEnv c => [(StratVar,Strategy)] -> Strat -> (Strat -> c x y) -> c x y
let_ ss body interp = proc a -> do
  let ss' = [ (v,Closure s' M.empty) | (v,s') <- ss ]
  senv <- readStratEnv -< ()
  localStratEnv (interp body) -< (a,M.union (M.fromList ss') senv) 
{-# INLINE let_ #-}

call :: (ArrowTry c, ArrowChoice c, ArrowApply c, HasTermEnv t env c, HasStratEnv c)
     => StratVar
     -> [Strat]
     -> [TermVar]
     -> (Strat -> c x y)
     -> c x y
call f actualStratArgs actualTermArgs interp = proc a -> do
  senv <- readStratEnv -< ()
  case M.lookup f senv of
    Just (Closure (Strategy formalStratArgs formalTermArgs body) senv') -> do
      tenv <- getTermEnv -< ()
      mapA bindTermArg -< zip actualTermArgs formalTermArgs
      let senv'' = bindStratArgs (zip formalStratArgs actualStratArgs)
                                 (if M.null senv' then senv else senv')
      b <- localStratEnv (interp body) -<< (a,senv'')
      tenv' <- getTermEnv -< ()
      putTermEnv <<< unionTermEnvs -< (formalTermArgs,tenv,tenv')
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

convertToList :: (ArrowChoice c, HasTerm t c) => c [t] t
convertToList = proc ts -> case ts of
  (x:xs) -> do
    l <- convertToList -< xs
    cons -< ("Cons",[x,l])
  [] -> cons -< ("Nil",[])
    
convertFromList :: (ArrowChoice c, ArrowTry c, HasTerm t c) => c t (Maybe [t])
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

equal :: (ArrowChoice c, ArrowAppend c, ArrowTry c, HasTerm t c, Lattice (Complete t) c) => c (t,t) t
equal = proc (t1,t2) -> do
  m <- matchTerm *** matchTerm -< (t1,t2)
  case m of
    (Cons c ts,Cons c' ts')
        | c == c' && eqLength ts ts' -> do
        ts'' <- zipWithA equal -< (ts,ts')
        cons -< (c,ts'')
        | otherwise -> fail -< ()
    (StringLiteral s, StringLiteral s')
        | s == s' -> success -< t1
        | otherwise -> fail -< ()
    (NumberLiteral n, NumberLiteral n')
        | n == n' -> success -< t1
        | otherwise -> fail -< ()
    (Wildcard, t) -> fail <+> term -< t
    (t, Wildcard) -> fail <+> term -< t
    (_,_) -> fail -< ()
 
match :: (ArrowChoice c, ArrowAppend c, ArrowTry c, HasTerm t c, HasTermEnv env t c, Lattice (Complete t) c)
      => c (TermPattern,t) t
match = proc (p,t) -> case p of
  S.As v p2 -> do
    t' <- match -< (S.Var v,t)
    match -< (p2,t')
  S.Var "_" -> success -< t
  S.Var x -> do
    m <- lookupTermVar -< x
    case m of
      Just t' -> do
        t'' <- equal -< (t,t')
        success -< t''
      Nothing -> do
        insertTerm -< (x,t)
        returnA -< t
  S.Cons c ts -> do
    m <- matchTermAgainstConstructor -< (c,t)
    case m of
      T.Cons c' ts'
        | eqLength ts ts' -> do
            ts'' <- zipWithA match -< (ts,ts')
            T.cons -< (c',ts'')
        | otherwise -> fail -< ()
      T.Wildcard -> do
        l <- mapA wildcard -< [() | _ <- ts]
        ts'' <- zipWithA match -< (ts,l)
        fail <+> cons -< (c,ts'')
      _ -> fail -< ()
  S.Explode c ts -> do
    m <- matchTermRefine -< t
    case m of
      T.Cons (Constructor c') ts' -> do
        s <- stringLiteral -< c'
        match -< (c,s)
        l <- convertToList -< ts'
        match -< (ts, l)
        success -< t
      T.StringLiteral _ -> do
        l <- convertToList -< []
        match -< (ts, l)
        success -< t
      T.NumberLiteral _ -> do
        l <- convertToList -< []
        match -< (ts, l)
        success -< t
      T.Wildcard ->
        (do
          w <- wildcard -< ()
          match -< (c,  w)
          w' <- wildcard -< ()
          match -< (ts, w')
          success -< t)
        <+>
        (do
          l <- convertToList -< []
          match -< (ts, l)
          success -< t)
      _ -> fail -< ()
  S.StringLiteral s -> do
    m <- matchTermRefine -< t
    case m of
      T.StringLiteral s'
        | s == s' -> success -< t
        | otherwise -> fail -< ()
      T.Wildcard -> fail <+> stringLiteral -< s
      _ -> fail -< ()
  S.NumberLiteral n -> do
    m <- matchTerm -< t
    case m of
      T.NumberLiteral n'
        | n == n' -> success -< t
        | otherwise -> fail -< ()
      T.Wildcard -> fail <+> numberLiteral -< n
      _ -> fail -< ()

build :: (ArrowChoice c, ArrowAppend c, ArrowTry c, HasTerm t c, HasTermEnv env t c, Lattice (Complete t) c)
      => c TermPattern t
build = proc p -> case p of
  S.As _ _ -> error "As-pattern in build is disallowed" -< ()
  S.Var x -> do
    m <- lookupTermVar -< x
    case m of
      Just t -> returnA -< t
      Nothing -> fail -< ()
  S.Cons c ts -> do
    ts' <- mapA build -< ts
    cons -< (c,ts')
  S.Explode c ts -> do
    m <- matchTerm <<< build -< c
    case m of
      T.StringLiteral s -> do
        ts' <- build -< ts
        ts'' <- convertFromList -< ts'
        case ts'' of
          Just tl -> cons -< (Constructor s,tl)
          Nothing -> fail <+> wildcard -< ()
      T.Wildcard -> fail <+> wildcard -< ()
      _ -> fail -< ()
  S.NumberLiteral n -> numberLiteral -< n
  S.StringLiteral s -> stringLiteral -< s
