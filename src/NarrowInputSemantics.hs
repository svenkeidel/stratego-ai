{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module NarrowInputSemantics where

import           Prelude hiding (id,fail,concat,sequence,all,zipWith,(.))

import           Result
import           Syntax hiding (Fail,TermPattern(..))
import           Syntax (TermPattern)
import qualified Syntax as S
import           Interpreter
import           WildcardSemantics(Term(..),TermEnv,Fuel,dedup')
import           WildcardTraceSemantics

import           Control.Category
import           Control.Arrow

import           Data.Semigroup ((<>))
import           Data.Hashable
import           Data.Sequence (Seq)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M

analyse :: Trace -> Seq Term
analyse ((t,env,s0) : tr) = case s0 of
  S.Fail -> mempty
  Id -> return t
  GuardedChoice s1 s2 s3 -> _
  Seq s1 s2 -> return t
  One s -> undefined
  Some s -> undefined
  All s -> undefined
  Scope xs s -> _
  Match f -> match f t env
  Build f -> _
  Let ss body -> _
  Call f ss ps -> _

match :: TermPattern -> Term -> TermEnv -> Seq Term
match p t env = case p of
  S.Var "_" -> return t
  S.Var x -> _
  S.Cons c ts -> case t of
    Cons c' ts'
      | c == c' && length ts == length ts' -> do
          ts'' <- zipWith match -< (ts,ts')
          success -< Cons c ts''
      | otherwise -> fail -< ()
    Wildcard -> do
      ts'' <- fail <+> zipWith match -< (ts,[Wildcard | _ <- ts])
      success -< Cons c ts''
    _ -> fail -< ()
  S.Explode c ts -> case t of
    Cons (Constructor c') ts' -> do
      match -< (c,StringLiteral c')
      match -< (ts, convertToList ts')
      success -< t
    StringLiteral _ -> do
      match -< (ts, convertToList [])
      success -< t
    NumberLiteral _ -> do
      match -< (ts, convertToList [])
      success -< t
    Wildcard ->
      (do
        match -< (c,  Wildcard)
        match -< (ts, Wildcard)
        success -< t)
      <+>
      (do
        match -< (ts, convertToList [])
        success -< t)
  S.StringLiteral s -> case t of
    StringLiteral s'
      | s == s' -> success -< t
      | otherwise -> fail -< ()
    Wildcard -> fail <+> success -< StringLiteral s
    _ -> fail -< ()
  S.NumberLiteral n -> case t of
    NumberLiteral n'
      | n == n' -> success -< t
      | otherwise -> fail -< ()
    Wildcard -> fail <+> success -< NumberLiteral n
    _ -> fail -< ()

