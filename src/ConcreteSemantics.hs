{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
module ConcreteSemantics where

import Prelude hiding (fail,sequence,all,uncurry,zipWith)

import Term (Constructor,TermVar,Term)
import qualified Term as T
import Syntax hiding (Fail)
import qualified Syntax as S
--import Interpreter
import Cat

import Control.Arrow

import Data.Map (Map)
import qualified Data.Map as M

data ClosedTerm = Cons Constructor [ClosedTerm] deriving Eq

instance Show ClosedTerm where
  show (Cons c ts) = show c ++ if null ts then "" else show ts

type TermEnv = Map TermVar ClosedTerm

interp :: (Interpreter TermEnv p,ArrowChoice p,ArrowPlus p,ArrowApply p) => Strat -> p ClosedTerm ClosedTerm
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
  Cong c ss -> lift (cong c ss (uncurry interp))
  One s -> lift (one (interp s))
  Some s -> lift (some (interp s))
  All s -> lift (all (interp s))
  Scope xs s -> scope xs (interp s)
  Match f -> proc t -> match -< (f,t)
  Build f -> proc _ -> build -< f

match :: (Interpreter TermEnv p,ArrowChoice p) => p (Term TermVar,ClosedTerm) ClosedTerm
match = proc (f,t@(Cons c ts)) -> case f of
  T.Var x -> do
    env <- getTermEnv -< ()
    case M.lookup x env of
      Just t' | t' == t -> success -< t
              | otherwise -> fail -< ()
      Nothing -> do
        putTermEnv -< env
        success -< t
  T.Cons c' ts'
    | c /= c' || length ts /= length ts' -> fail -< ()
    | otherwise -> do
       ts'' <- zipWith match -< (ts',ts)
       returnA -< Cons c ts''

build :: (Interpreter TermEnv p, ArrowChoice p) => p (Term TermVar) ClosedTerm
build = proc f -> case f of
  (T.Var x) -> do
    env <- getTermEnv -< ()
    case M.lookup x env of
      Nothing -> fail -< ()
      Just t -> success -< t
  (T.Cons c ts) -> do
    ts' <- mapA build -< ts
    returnA -< (Cons c ts')

lift :: Arrow p => p (Constructor,[ClosedTerm]) (Constructor,[ClosedTerm]) -> p ClosedTerm ClosedTerm
lift p = proc (Cons c ts) -> do
  (c',ts') <- p -< (c,ts)
  returnA -< (Cons c' ts')

