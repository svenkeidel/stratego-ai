{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WildcardSemanticsDelayed where

import Prelude hiding (id,fail,concat,sequence,all,zipWith,(.))
import WildcardSemantics

import Result
import Syntax hiding (Fail)
import Interpreter

import Control.Category
import Control.Arrow hiding ((<+>))
import Control.Arrow.Operations
import Control.Arrow.Transformer.Power
import Control.Arrow.Transformer.Deduplicate

eval :: Int -> StratEnv -> Strat -> (Term,TermEnv) -> Pow (Result (Term,TermEnv))
eval i senv s = runInterp $ eval' i senv s

newtype Interp a b = Interp {runInterp :: (a,TermEnv) -> Pow (Result (b,TermEnv))}

instance Category Interp where
  id = Interp $ \a -> return (Success a)
  f . g = Interp $ \a -> do
    b <- runInterp g a
    case b of
        Success b' -> runInterp f b'
        Fail -> return Fail

instance Try Interp where
  fail = Interp $ \_ -> return Fail
  try f g h = Interp $ \a -> do
    b <- runInterp f a
    case b of
      Success b' -> runInterp g b'
      Fail -> runInterp h a

instance Arrow Interp where
  arr f = Interp $ \(a,e) -> return $ Success (f a, e)
  first f = Interp $ \((a,b),e) -> (fmap.fmap) (\(c,e') -> ((c,b),e')) (runInterp f (a,e))
  second f = Interp $ \((a,b),e) -> (fmap.fmap) (\(c,e') -> ((a,c),e')) (runInterp f (b,e))

instance ArrowChoice Interp where
  left f = Interp $ \(a,e) -> case a of
    Left b -> (fmap.fmap) (first Left) (runInterp f (b,e))
    Right c -> return $ Success (Right c,e)
  right f = Interp $ \(a,e) -> case a of
    Left c -> return $ Success (Left c,e)
    Right b -> (fmap.fmap) (first Right) (runInterp f (b,e))
  f +++ g = Interp $ \(a,e) -> case a of
    Left b  -> (fmap.fmap) (first Left)  (runInterp f (b,e))
    Right b -> (fmap.fmap) (first Right) (runInterp g (b,e))

instance ArrowAlternative Interp where
  -- zeroArrow = Interp (const mempty)
  f <+> g = Interp $ \x -> runInterp f x `union` runInterp g x

instance ArrowApply Interp where
  app = Interp $ \((f,x),tenv) -> runInterp f (x,tenv)
        
instance Deduplicate Interp where
  dedup f = Interp $ \a -> dedup' $ runInterp f a

instance ArrowState TermEnv Interp where
  fetch = Interp $ \(_,e) -> return (return (e,e))
  store = Interp $ \(e,_) -> return (return ((),e))

