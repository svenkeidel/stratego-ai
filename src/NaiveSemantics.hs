{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module NaiveSemantics where

import Prelude hiding (fail)

import Syntax hiding (Fail)
import qualified Syntax as S
import Interpreter

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.State hiding (fail)
import Control.Monad.Reader hiding (fail)

import Data.Text (unpack)

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Lazy.Merge as M

newtype Multiple a = Multiple {unMultilpe :: [Result a]}

instance Show a => Show (Multiple a) where
  show (Multiple a) = show a

instance Functor Multiple where
  fmap f (Multiple l) = Multiple (fmap (fmap f ) l)

instance Applicative Multiple where
  pure = return
  (<*>) = ap

instance Alternative Multiple where
  empty = mzero
  (<|>) = mplus

instance Monad Multiple where
  return = Multiple . return . return
  Multiple l >>= k = Multiple $ do
    a <- l
    case a of
      Success b -> unMultilpe (k b)
      Fail -> [Fail]

instance MonadPlus Multiple where
  mzero = Multiple mzero
  (Multiple x) `mplus` (Multiple y) = Multiple (x `mplus` y)

instance Interpreter Multiple where
  success = Multiple . return . success
  fail = Multiple $ return fail
  result (Multiple r) = Multiple $ fmap return r

data Term = Cons Constructor [Term]
          | Hole
          deriving Eq

instance Show Term where
  show (Cons c ts) = unpack c ++ if null ts then "" else show ts
  show Hole = "_"

type TermEnv = Map Var Term

interp :: Strat -> Term -> Interp StratEnv TermEnv Multiple Term
interp s0 t0 = case s0 of
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
  Path i s -> case t0 of
    Cons c ts -> Cons c <$> nth i (interp s) ts
    Hole -> fail `mplus` success Hole
  Cong c' ss0 -> case t0 of
    Cons c ts0 ->
      if c /= c' || length ts0 /= length ss0
      then fail
      else let go (s:ss) (t:ts) = do
                 t' <- interp s t
                 ts' <- go ss ts
                 return (t':ts')
               go _ _ = return []
           in Cons c <$> go ss0 ts0
    Hole -> fail `mplus` interp (Cong c' ss0) (Cons c' [Hole | _ <- ss0])
  One s -> case t0 of
    Cons c ts0 -> msum [Cons c <$> nth i (interp s) ts0 | i <- [1..length ts0]]
    Hole -> fail `mplus` success Hole
  Some s -> case t0 of
    Cons c ts0 -> 
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
    Hole -> fail `mplus` success Hole
  All s -> case t0 of
    Cons c ts0 -> 
      let go (t:ts) = do
            t' <- interp s t
            ts' <- go ts
            success (t':ts')
          go [] = return []
      in Cons c <$> go ts0
    Hole -> fail `mplus` success Hole
  Match f -> match f t0
  Build f -> build f
  -- Where _ -> undefined
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

    nth :: (Monad f,Interpreter f) => Int -> (a -> f a) -> ([a] -> f [a])
    nth 1 f (x:xs) = do
      x' <- f x
      return (x':xs)
    nth i f (x:xs) = do
      xs' <- nth (i-1) f xs
      return (x:xs')
    nth _ _ [] = fail

match :: TermV -> Term -> Interp StratEnv TermEnv Multiple Term
match (VarV x) t = do
  env <- get
  case M.lookup x env of
    Just t' | ground t && ground t' && t' == t -> success t
            | otherwise -> fail `mplus` unify t t'
    Nothing -> do
      put $ M.insert x t env
      success t
match (ConsV c ts) (Cons c' ch)
  | c' /= c || length ch /= length ts = fail
  | otherwise = Cons c' <$> zipWithM match ts ch
match t@(ConsV c ts) Hole = fail `mplus` match t (Cons c [Hole | _ <- ts ])

ground :: Term -> Bool
ground (Cons _ ts) = all ground ts
ground Hole = False

unify :: (Monad f,Interpreter f) => Term -> Term -> f Term
unify (Cons c ts) (Cons c' ts')
  | c /= c' || length ts /= length ts' = fail
  | otherwise = Cons c <$> zipWithM unify ts ts'
unify Hole t = return t
unify t Hole = return t

build :: TermV -> Interp StratEnv TermEnv Multiple Term
build (VarV x) = do
  env <- get
  case M.lookup x env of
    Nothing -> fail
    Just t -> success t
build (ConsV c ts) =
  Cons c <$> mapM build ts
