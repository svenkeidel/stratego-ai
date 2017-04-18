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
  Interp f . Interp g = Interp $ \a -> do
    b <- g a
    case b of
        Success b' -> f b'
        Fail -> return Fail

instance Try Interp where
  fail = Interp $ \_ -> return Fail
  try (Interp f) (Interp g) (Interp h) = Interp $ \a -> do
    b <- f a
    case b of
      Success b' -> g b'
      Fail -> h a

instance Arrow Interp where
  arr f = Interp $ \(a,e) -> return $ Success (f a, e)
  first (Interp f) = Interp $ \((a,b),e) -> (fmap.fmap) (\(c,e') -> ((c,b),e')) (f (a,e))
  second (Interp f) = Interp $ \((a,b),e) -> (fmap.fmap) (\(c,e') -> ((a,c),e')) (f (b,e))

instance ArrowChoice Interp where
  left (Interp f) = Interp $ \(a,e) -> case a of
    Left b -> (fmap.fmap) (first Left) (f (b,e))
    Right c -> return $ Success (Right c,e)
  right (Interp f) = Interp $ \(a,e) -> case a of
    Left c -> return $ Success (Left c,e)
    Right b -> (fmap.fmap) (first Right) (f (b,e))
  (Interp f) +++ (Interp g) = Interp $ \(a,e) -> case a of
    Left b  -> (fmap.fmap) (first Left)  (f (b,e))
    Right b -> (fmap.fmap) (first Right) (g (b,e))

instance ArrowAppend Interp where
  --zeroArrow = Interp (const mempty)
  Interp f <+> Interp g = Interp $ \x -> f x `union` g x

instance ArrowApply Interp where
  app = Interp $ \((f,x),tenv) -> runInterp f (x,tenv)
        
instance Deduplicate Interp where
  dedup (Interp f) = Interp $ \a -> dedup' $ f a

instance ArrowState TermEnv Interp where
  fetch = Interp $ \(_,e) -> return (return (e,e))
  store = Interp $ \(e,_) -> return (return ((),e))

