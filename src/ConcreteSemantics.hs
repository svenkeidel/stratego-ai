{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes#-}
module ConcreteSemantics where

import Prelude hiding (id,(.),fail,sequence,all,uncurry,zipWith)

import Term (Constructor,TermVar,Term)
import qualified Term as T
import Syntax hiding (Fail)
import qualified Syntax as S
import Interpreter
import Classes

import Control.Monad hiding (fail,sequence)
import Control.Category
import Control.Arrow

import Data.Map (Map)
import qualified Data.Map as M


data ClosedTerm = Cons Constructor [ClosedTerm] deriving Eq
data Result a = Success a | Fail
newtype Interp a b = Interp {runInterp :: (a,Environment) -> Result (b,Environment)}

interp :: StratEnv -> Strat -> Interp ClosedTerm ClosedTerm
interp env s0 = case s0 of
  Test s -> test (interp env s)
  Neg s -> neg (interp env s)
  S.Fail -> fail
  Id -> success
  Seq s1 s2 -> sequence (interp env s1) (interp env s2)
  Choice s1 s2 -> choice (interp env s1) (interp env s2)
  LeftChoice s1 s2 -> leftChoice (interp env s1) (interp env s2)
  Rec x s -> recur env x s (`interp` s)
  RecVar x -> var env x (interp env)
  Path i s -> lift (path i (interp env s))
  Cong c ss -> lift (cong c (interp env <$> ss))
  One s -> lift (one (interp env s))
  Some s -> lift (some (interp env s))
  All s -> lift (all (interp env s))
  Scope xs s -> scope xs (interp env s)
  Match f -> proc t -> match -< (f,t)
  Build f -> proc _ -> build -< f

match :: (CCC p, Try p, TermEnv Env p, HasLists p) => p (Term TermVar,ClosedTerm) ClosedTerm
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

build :: (CCC p, Try p,TermEnv (Map TermVar ClosedTerm) p, HasLists p) => p (Term TermVar) ClosedTerm
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
  returnA -< Cons c' ts'

-- Instances -----------------------------------------------------------------------------------------

type Env = Map TermVar ClosedTerm
data Environment = Singleton Env | Product Environment Environment

instance Show ClosedTerm where
  show (Cons c ts) = show c ++ if null ts then "" else show ts

instance Functor Result where
  fmap f (Success a) = Success (f a)
  fmap _ Fail = Fail

instance Applicative Result where
  pure = return
  (<*>) = ap

instance Monad Result where
  return = Success
  f >>= k = case f of
    Success a -> k a
    Fail -> Fail

instance Category Interp where
  id = Interp Success
  f . g = Interp $ runInterp g >=> runInterp f

instance Arrow Interp where
  arr f = Interp (\(a,e) -> Success (f a, e))
  first f = Interp $ \((a,b),e) -> fmap (\(c,e') -> ((c,b),e')) (runInterp f (a,e))
  second f = Interp $ \((a,b),e) -> fmap (\(c,e') -> ((a,c),e')) (runInterp f (b,e))
  f &&& g = Interp $ \x -> case (runInterp f x,runInterp g x) of
    (Success (b,e'),Success (c,e'')) -> Success ((b,c),Product e' e'')
    (_,_) -> Fail
  f *** g = Interp $ \((a,b),e) -> case (runInterp f (a,e), runInterp g (b,e)) of
    (Success (c,e'),Success (d,e'')) -> Success ((c,d),Product e' e'')
    (_,_) -> Fail

instance Products Interp where
  p1 = Interp $ \((a,_),e) -> case e of
    Product e1 _ -> Success (a,e1)
    _ -> Fail
  p2 = Interp $ \((_,b),e) -> case e of
    Product _ e2 -> Success (b,e2)
    _ -> Fail

instance ArrowChoice Interp where
  left f = Interp $ \(a,e) -> case a of
    Left b -> fmap (first Left) (runInterp f (b,e))
    Right c -> Success (Right c,e)
  right f = Interp $ \(a,e) -> case a of
    Left c -> Success (Left c,e)
    Right b -> fmap (first Right) (runInterp f (b,e))
  f +++ g = Interp $ \(a,e) -> case a of
    Left b  -> fmap (first Left)  (runInterp f (b,e))
    Right b -> fmap (first Right) (runInterp g (b,e))

instance Try Interp where
  success = Interp Success
  fail = Interp (const Fail)
  try t s f = Interp $ \(a,e) -> case runInterp t (a,e) of
    Success (b,e') -> runInterp s (b,e')
    Fail -> runInterp f (a,e)

instance ArrowZero Interp where
  zeroArrow = Interp (const Fail)

instance ArrowPlus Interp where
  f <+> g = Interp $ \x -> case (runInterp f x,runInterp g x) of
    (Success y,_) -> Success y
    (_,Success y) -> Success y
    (_,_) -> Fail

instance TermEnv (Map TermVar ClosedTerm) Interp where
  getTermEnv = Interp $ \((),e) -> case e of
    Singleton e' -> Success (e',e)
    _ -> Fail
  putTermEnv = Interp $ \(e,_) -> Success ((),Singleton e)

instance HasLists Interp where
  foldList f g = Interp $ \(xs,e) -> case xs of
    (a:as) -> do
      (b,e') <- runInterp (foldList f g) (as,e)
      runInterp g ((a,b),e')
    [] -> runInterp f ((),e)

instance HasNumbers Interp where
  foldNat f g = Interp $ \(n,e) -> case n of
    0 -> runInterp f ((),e)
    _ -> (runInterp (foldNat f g) >=> runInterp g) (n-1,e)

instance Exponentials Interp where
  curry f = Interp $ \(a,e) -> return (Interp $ \(b,e') -> runInterp f ((a,b),e'), e)
  uncurry f = Interp $ \((a,b),e) -> do
    (f',e') <- runInterp f (a,e)
    runInterp f' (b,e')
  eval = Interp $ \((f,a),e) -> runInterp f (a,e)
