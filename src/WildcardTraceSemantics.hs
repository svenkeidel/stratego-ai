{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module WildcardTraceSemantics where

import           Prelude hiding (id,fail,concat,sequence,all,zipWith,(.))

import           Result
import           Syntax hiding (Fail,TermPattern(..))
import qualified Syntax as S
import           Interpreter

import           WildcardSemantics(Term(..),TermEnv,lift,match,build,dedup')

import           Control.Arrow
import           Control.Category

import           Data.Semigroup ((<>))
import           Data.Sequence (Seq)
import           Data.Hashable

type Trace = [(Term, TermEnv, Strat)]

newtype Interp a b = Interp {runInterp :: StratEnv -> (a,(TermEnv,Trace)) -> Seq (Result (b,(TermEnv,Trace)))}

eval :: Int -> Strat -> StratEnv -> (Term,TermEnv) -> Seq (Result (Term,(TermEnv,Trace)))
eval fuel s senv (t,tenv) = runInterp (interp fuel s) senv (t,(tenv,mempty))

type Fuel = Int

interp :: Fuel -> Strat -> Interp Term Term
interp 0 _ = proc _ -> fail <+> success -< Wildcard
interp i s0 = dedup $ trace s0 <<< case s0 of
  S.Fail -> fail
  Id -> id
  GuardedChoice s1 s2 s3 -> guardedChoice (interp i s1) (interp i s2) (interp i s3)
  Seq s1 s2 -> sequence (interp i s1) (interp i s2)
  One s -> lift (one (interp i s))
  Some s -> lift (some (interp i s))
  All s -> lift (all (interp i s))
  Scope xs s -> scope xs (interp i s)
  Match f -> proc t -> match -< (f,t)
  Build f -> proc _ -> build -< f
  Let ss body -> let_ ss (interp i body)
  Call f ss ps -> call f ss ps (interp (i-1))

dedup :: (Hashable b,Eq b) => Interp a b -> Interp a b
dedup f = Interp $ \senv x -> dedup' $ runInterp f senv x

trace :: Strat -> Interp Term Term
trace s = Interp $ \_ (t,(e,tr)) -> return (Success (t,(e, (t,e,s) : tr)))

-- Instances -----------------------------------------------------------------------------------------

instance Category Interp where
  id = Interp (const (return . Success))
  f . g = Interp $ \senv x -> do
            y <- runInterp g senv x
            case y of
              Success t -> runInterp f senv t
              Fail -> return Fail

instance Arrow Interp where
  arr f = Interp (\_ (a,e) -> return $ Success (f a, e))
  first f = Interp $ \senv ((a,b),e) -> (fmap.fmap) (\(c,e') -> ((c,b),e')) (runInterp f senv (a,e))
  second f = Interp $ \senv ((a,b),e) -> (fmap.fmap) (\(c,e') -> ((a,c),e')) (runInterp f senv (b,e))

instance ArrowChoice Interp where
  left f = Interp $ \senv (a,e) -> case a of
    Left b -> (fmap.fmap) (first Left) (runInterp f senv (b,e))
    Right c -> return $ Success (Right c,e)
  right f = Interp $ \senv (a,e) -> case a of
    Left c -> return $ Success (Left c,e)
    Right b -> (fmap.fmap) (first Right) (runInterp f senv (b,e))
  f +++ g = Interp $ \senv (a,e) -> case a of
    Left b  -> (fmap.fmap) (first Left)  (runInterp f senv (b,e))
    Right b -> (fmap.fmap) (first Right) (runInterp g senv (b,e))

instance Try Interp where
  success = Interp (const (return . Success))
  fail = Interp (const (const (return Fail)))
  try t s f = Interp $ \senv (a,e) -> do
    x <- runInterp t senv (a,e)
    case x of
      Success y -> runInterp s senv y
      Fail -> runInterp f senv (a,e)

instance ArrowZero Interp where
  zeroArrow = Interp (const (const mempty))

instance ArrowPlus Interp where
  f <+> g = Interp $ \x -> runInterp f x <> runInterp g x

instance ArrowApply Interp where
  app = Interp $ \senv ((f,x),tenv) -> runInterp f senv (x,tenv)

instance HasTermEnv TermEnv Interp where
  getTermEnv = Interp $ \_ ((),(e,tr)) -> return $ Success (e,(e,tr))
  putTermEnv = Interp $ \_ (e,(_,tr)) -> return $ Success ((),(e,tr))

instance HasStratEnv Interp where
  readStratEnv = Interp $ \senv (_,tenv) -> return (Success (senv,tenv))
  localStratEnv f = Interp $ \_ ((x,senv),tenv) -> runInterp f senv (x,tenv)
