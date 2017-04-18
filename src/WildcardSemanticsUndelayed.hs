{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WildcardSemanticsUndelayed where

import Prelude hiding (fail)
import WildcardSemantics 

import Syntax hiding (Fail)
import Interpreter
import UncertainResult

import Control.Category
import Control.Arrow hiding ((<+>))
import Control.Arrow.Operations
import Control.Arrow.Transformer.Deduplicate

newtype Interp a b = Interp { runInterp :: (a,TermEnv) -> UncertainResult (b,TermEnv) }

eval :: Int -> StratEnv -> Strat -> (Term,TermEnv) -> UncertainResult (Term,TermEnv)
eval i senv s = runInterp $ eval' i senv s

instance Category Interp where
  id = Interp $ \a -> Success a
  Interp f . Interp g = Interp $ \a ->
    case g a of
      Success b' -> f b'
      SuccessOrFail b' -> case f b' of
        Success c -> SuccessOrFail c
        SuccessOrFail c -> SuccessOrFail c
        Fail -> Fail
      Fail -> Fail

instance Try Interp where
  fail = Interp $ \_ -> Fail
  try (Interp f) (Interp g) (Interp h) = Interp $ \a ->
    case f a of
      Success b -> g b 
      SuccessOrFail b' -> g b' `mappend` h a
      Fail -> h a

instance Arrow Interp where
  arr f = Interp (\(a,e) -> Success (f a, e))
  first f = Interp $ \((a,b),e) -> fmap (\(c,e') -> ((c,b),e')) (runInterp f (a,e))
  second f = Interp $ \((a,b),e) -> fmap (\(c,e') -> ((a,c),e')) (runInterp f (b,e))

instance ArrowChoice Interp where
  left (Interp f) = Interp $ \(a,e) -> case a of
    Left b -> first Left <$> f (b,e)
    Right c -> Success (Right c,e)
  right (Interp f) = Interp $ \(a,e) -> case a of
    Left c -> Success (Left c,e)
    Right b -> first Right <$> f (b,e)
  Interp f +++ Interp g = Interp $ \(a,e) -> case a of
    Left b  -> first Left  <$> f (b,e)
    Right b -> first Right <$> g (b,e)
  Interp f ||| Interp g = Interp $ \(a,e) -> case a of
    Left b  -> f (b,e)
    Right b -> g (b,e)


instance ArrowAppend Interp where
  -- zeroArrow = fail
  f <+> g = Interp $ \x -> runInterp f x `mappend` runInterp g x

instance ArrowState TermEnv Interp where
  fetch = Interp $ \(_,e) -> Success (e,e)
  store = Interp $ \(e,_) -> Success ((),e)

instance ArrowApply Interp where
  app = Interp $ \((f,b),e) -> runInterp f (b,e)

instance Deduplicate Interp where
  dedup f = f
