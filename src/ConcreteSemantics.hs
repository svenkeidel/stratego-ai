{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ConcreteSemantics where

import Prelude hiding (fail)
import Data.Text (unpack)

import Syntax hiding (Fail)
import qualified Syntax as S
import Interpreter

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.State hiding (fail)
import Control.Monad.Reader hiding (fail)

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Lazy.Merge as M


data Term = Cons Constructor [Term] deriving Eq

instance Show Term where
  show (Cons c ts) = unpack c ++ if null ts then "" else show ts

type TermEnv = Map Var Term

interp :: Strat -> Term -> Interp StratEnv TermEnv Result Term
interp s0 t0@(Cons c ts0) = case s0 of
  Test s -> try (interp s t0) $ \r -> case r of
    Success _ -> success t0
    Fail -> fail
  Neg s -> do
    env <- get
    try (interp s t0) $ \r -> case r of
      Success _ -> fail
      Fail -> do
        put env
        success t0
  S.Fail -> fail
  Id -> success t0
  Seq s1 s2 -> interp s1 t0 >>= interp s2
  Choice s1 s2 -> interp s1 t0 `mplus` interp s2 t0
  LeftChoice s1 s2 -> try2 (interp s1 t0) (interp s2 t0) $ \r1 r2 -> case (r1,r2) of
    (Success x,_) -> success x
    (Fail,Success x) -> success x
    (Fail,Fail) -> fail
  Rec x s -> local (M.insert x s) (interp s t0)
  Var x -> do
    m <- reader (M.lookup x)
    case m of
      Just s -> interp s t0
      Nothing -> error "Recursion variable was not in scope"
  Path i s ->
    Cons c <$> nth i (interp s) ts0
  Cong c' ss0 ->
    if c /= c' || length ts0 /= length ss0
    then fail
    else let go (s:ss) (t:ts) = do
               t' <- interp s t
               ts' <- go ss ts
               return (t':ts')
             go _ _ = return []
         in Cons c <$> go ss0 ts0
  One s ->
    msum [Cons c <$> nth i (interp s) ts0 | i <- [1..length ts0]]
  Some s -> 
    let go (t:ts) = try (interp s t) $ \m -> case m of
          Success t' -> do
            (_,ts') <- go ts
            success (True,t':ts')
          Fail -> do
            (b,ts') <- go ts
            success (b,t:ts')
        go [] = success (False,[])
    in do
      (oneSucceeded,ts') <- go ts0
      if oneSucceeded
        then success $ Cons c ts'
        else fail
  All s -> 
    let go (t:ts) = do
          t' <- interp s t
          ts' <- go ts
          success (t':ts')
        go [] = return []
    in Cons c <$> go ts0
  Match f -> match f t0
  Build f -> build f
  Where _ -> undefined
  Scope xs s -> do
    env <- get
    put (foldr M.delete env xs)
    t' <- interp s t0
    env' <- get
    put $ merge xs env' env
    return t'
  where
    merge xs = M.merge M.preserveMissing M.dropMissing $ M.zipWithMatched $ \k x y ->
                 if k `elem` xs then y else x

    nth :: Int -> (a -> Interp r s Result a) -> ([a] -> Interp r s Result [a])
    nth 1 f (x:xs) = do
      x' <- f x
      return (x':xs)
    nth i f (x:xs) = do
      xs' <- nth (i-1) f xs
      return (x:xs')
    nth _ _ [] = fail

match :: TermV -> Term -> Interp StratEnv TermEnv Result Term
match (VarV x) t = do
  env <- get
  case M.lookup x env of
    Just t' | t' == t -> success t
            | otherwise -> fail
    Nothing -> do
      put $ M.insert x t env
      success t
match (ConsV c ts) (Cons c' ch)
  | c' /= c || length ch /= length ts = fail
  | otherwise = Cons c' <$> zipWithM match ts ch

build :: TermV -> Interp StratEnv TermEnv Result Term
build (VarV x) = do
  env <- get
  case M.lookup x env of
    Nothing -> fail
    Just t -> success t
build (ConsV c ts) =
  Cons c <$> mapM build ts
