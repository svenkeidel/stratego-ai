{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HoleSemantics where

import Prelude hiding (fail,concat,sequence,all)
import qualified Prelude as P

import Term (Term,Constructor,TermVar)
import qualified Term as T
import Syntax
import Interpreter hiding (Fail)
import Multiple (Multiple)

import Control.Monad hiding (fail,sequence)
import Control.Monad.State hiding (fail,sequence)

import Data.Sequence (Seq)
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

interp :: Strat -> PartialTerm -> Interp TermEnv (Multiple Seq) PartialTerm
interp s0 t = case s0 of
  Test s -> test interp s t
  Neg s -> neg interp s t
  Fail -> fail
  Id -> success t
  Seq s1 s2 -> sequence interp s1 s2 t
  Choice s1 s2 -> choice interp s1 s2 t
  LeftChoice s1 s2 -> leftChoice interp s1 s2 t
  Rec x s -> limit (recur interp x s t) Hole
  RecVar x -> limit (var interp x t) Hole
  Path i s -> case t of
    Cons c ts -> uncurry Cons <$> path interp i s c ts
    Hole -> fail `mplus` success Hole
  Cong c ss -> case t of
    Cons c' ts -> uncurry Cons <$> cong interp c ss c' ts
    Hole -> fail `mplus` interp (Cong c ss) (Cons c [Hole | _ <- ss])
  One s -> case t of
    Cons c ts -> uncurry Cons <$> one interp s c ts
    Hole -> fail `mplus` success Hole
  Some s -> case t of
    Cons c ts -> uncurry Cons <$> some interp s c ts
    Hole -> fail `mplus` success Hole
  All s -> case t of
    Cons c ts -> uncurry Cons <$> all interp s c ts
    Hole -> fail `mplus` success Hole
  Scope xs s -> scope interp xs s t
  Match f -> match f t
  Build f -> build f

match :: (MonadPlus f,CanFail f) => Term TermVar -> PartialTerm -> Interp TermEnv f PartialTerm
match (T.Var x) t = do
  env <- get
  case M.lookup x env of
    Just t' | groundEq t t' -> success t
            | otherwise -> fail `mplus` unify t t'
    Nothing -> do
      put $ M.insert x t env
      success t
match (T.Cons c ts) (Cons c' ch)
  | c' /= c || length ch /= length ts = fail
  | otherwise = Cons c' <$> zipWithM match ts ch
match (T.Cons c ts) Hole = fail `mplus` (Cons c <$> zipWithM match ts [Hole | _ <- ts ])

groundEq :: PartialTerm -> PartialTerm -> Bool
groundEq (Cons c ts) (Cons c' ts')
  | c == c' = P.all (uncurry groundEq) (zip ts ts') 
groundEq _ _ = False

unify :: (Monad f,CanFail f) => PartialTerm -> PartialTerm -> f PartialTerm
unify (Cons c ts) (Cons c' ts')
  | c /= c' || length ts /= length ts' = fail
  | otherwise = Cons c <$> zipWithM unify ts ts'
unify Hole t = return t
unify t Hole = return t

build :: (Monad f,CanFail f) => Term TermVar -> Interp TermEnv f PartialTerm
build (T.Var x) = do
  env <- get
  case M.lookup x env of
    Nothing -> fail
    Just t -> success t
build (T.Cons c ts) =
  Cons c <$> mapM build ts
