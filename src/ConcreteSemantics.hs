{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ConcreteSemantics where

import Prelude hiding (fail,sequence,all)
import Data.Text (unpack)

import Syntax hiding (Fail)
import qualified Syntax as S
import Interpreter

import Control.Monad hiding (fail,sequence)
import Control.Monad.State hiding (fail,sequence)

import Data.Map (Map)
import qualified Data.Map as M

data Term = Cons Constructor [Term] deriving Eq

instance Show Term where
  show (Cons c ts) = unpack c ++ if null ts then "" else show ts

type TermEnv = Map Var Term

interp :: (MonadPlus f,CanFail f) => Strat -> Term -> Interp TermEnv f Term
interp s0 t0@(Cons c ts0) = case s0 of
  Test s -> test interp s t0
  Neg s -> neg interp s t0
  S.Fail -> fail
  Id -> success t0
  Seq s1 s2 -> sequence interp s1 s2 t0
  Choice s1 s2 -> choice interp s1 s2 t0
  LeftChoice s1 s2 -> leftChoice interp s1 s2 t0
  Rec x s -> recur interp x s t0
  Var x -> var interp x t0
  Path i s -> uncurry Cons <$> path interp i s c ts0
  Cong c' ss0 -> uncurry Cons <$> cong interp c' ss0 c ts0
  One s -> uncurry Cons <$> one interp s c ts0
  Some s -> uncurry Cons <$> some interp s c ts0
  All s -> uncurry Cons <$> all interp s c ts0
  Scope xs s -> scope interp xs s t0
  Match f -> match f t0
  Build f -> build f

match :: (Monad f,CanFail f) => TermV -> Term -> Interp TermEnv f Term
match f t@(Cons c ts) = case f of
  VarV x -> do
    env <- get
    case M.lookup x env of
      Just t' | t' == t -> success t
              | otherwise -> fail
      Nothing -> do
        put $ M.insert x t env
        success t
  (ConsV c' ts')
    | c' /= c || length ts' /= length ts -> fail
    | otherwise -> Cons c' <$> zipWithM match ts' ts

build :: (Monad f,CanFail f) => TermV -> Interp TermEnv f Term
build (VarV x) = do
  env <- get
  case M.lookup x env of
    Nothing -> fail
    Just t -> success t
build (ConsV c ts) =
  Cons c <$> mapM build ts
