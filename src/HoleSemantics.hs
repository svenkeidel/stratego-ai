{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
module HoleSemantics where

import Prelude hiding (fail,concat,sequence,all,uncurry,zipWith)
import qualified Prelude as P

import Term (Term,Constructor,TermVar)
import qualified Term as T
import Syntax
import qualified Syntax as S
import Cat

import Control.Arrow

import Data.Map (Map)
import qualified Data.Map as M

data PartialTerm
    = Cons Constructor [PartialTerm]
    | Hole
    deriving Eq

instance Show PartialTerm where
  show (Cons c ts) = show c ++ if null ts then "" else show ts
  show Hole = "_"

type TermEnv = Map TermVar PartialTerm

interp :: (Interpreter TermEnv p,ArrowChoice p,ArrowPlus p,ArrowApply p) => Strat -> p PartialTerm PartialTerm
interp s0 = case s0 of
  Test s -> test (interp s)
  Neg s -> neg (interp s)
  S.Fail -> fail
  Id -> success
  Seq s1 s2 -> sequence (interp s1) (interp s2)
  Choice s1 s2 -> choice (interp s1) (interp s2)
  LeftChoice s1 s2 -> leftChoice (interp s1) (interp s2)
  Rec x s -> recur x s (interp s)
  RecVar x -> var x (uncurry interp)
  Path i s -> lift (path i (interp s))
  Cong c ss -> proc t -> case t of
    Cons c' ts' -> do
      (c'',ts'') <- cong c ss (uncurry interp) -< (c',ts')
      returnA -< Cons c'' ts''
    Hole -> fail <+> interp (Cong c ss) -< Cons c [Hole | _ <- ss]
  One s -> lift (one (interp s))
  Some s -> lift (some (interp s))
  All s -> lift (all (interp s))
  Scope xs s -> scope xs (interp s)
  Match f -> proc t -> match -< (f,t)
  Build f -> proc _ -> build -< f

lift :: (Interpreter env p,ArrowChoice p,ArrowPlus p) => p (Constructor,[PartialTerm]) (Constructor,[PartialTerm]) -> p PartialTerm PartialTerm
lift p = proc t -> case t of
  Cons c ts -> do
    (c',ts') <- p -< (c,ts)
    returnA -< (Cons c' ts')
  Hole ->
    fail <+> success -< Hole

match :: (Interpreter TermEnv p, ArrowChoice p,ArrowPlus p) => p (Term TermVar,PartialTerm) PartialTerm
match = proc (f,t) -> case f of
  T.Var x -> do
    env <- getTermEnv -< ()
    case M.lookup x env of
      Just t' | groundEq t t' -> success -< t
              | otherwise -> fail <+> success <<< unify -< (t,t')
      Nothing -> do
        putTermEnv -< env
        success -< t
  T.Cons c ts -> case t of
    Cons c' ts'
      | c /= c' || length ts /= length ts' -> fail -< ()
      | otherwise -> do
          ts'' <- zipWith match -< (ts,ts')
          returnA -< Cons c ts''
    Hole -> do
      ts'' <- fail <+> zipWith match -< (ts,[Hole | _ <- ts])
      returnA -< Cons c ts''

groundEq :: PartialTerm -> PartialTerm -> Bool
groundEq (Cons c ts) (Cons c' ts')
  | c == c' = P.all (uncurry groundEq) (zip ts ts') 
groundEq _ _ = False

unify :: (Interpreter env p,ArrowChoice p) => p (PartialTerm,PartialTerm) PartialTerm
unify = proc (t1,t2) -> case (t1,t2) of
  (Cons c ts,Cons c' ts')
    | c /= c' || length ts /= length ts' -> fail -< ()
    | otherwise -> do
        ts'' <- zipWith unify -< (ts,ts')
        returnA -< Cons c ts''
  (Hole, t) -> returnA -< t
  (t, Hole) -> returnA -< t


build :: (Interpreter TermEnv p, ArrowChoice p) => p (Term TermVar) PartialTerm
build = proc f -> case f of
  (T.Var x) -> do
    env <- getTermEnv -< ()
    case M.lookup x env of
      Nothing -> fail -< ()
      Just t -> success -< t
  (T.Cons c ts) -> do
    ts' <- mapA build -< ts
    returnA -< Cons c ts'
