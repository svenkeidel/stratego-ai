{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WildcardSemanticsDelayed where

import Prelude hiding (id,fail,concat,sequence,all,zipWith,(.))

import Interpreter
import Syntax hiding (Fail,TermPattern(..))
import Utils
import WildcardSemantics

import Data.Powerset
import Data.Result
import Data.Term(HasTerm(..))

import Control.Arrow hiding ((<+>))
import Control.Category

eval :: Int -> StratEnv -> Strat -> (Term,TermEnv) -> Pow (Result (Term,TermEnv))
eval i senv s = runInterp (eval' i s) senv
{-# INLINE eval #-}

newtype Interp a b = Interp {runInterp :: StratEnv -> (a,TermEnv) -> Pow (Result (b,TermEnv))}

instance HasTerm Term Interp where
  matchTerm = arr matchTermDefault
  {-# INLINE matchTerm #-}
  term = arr termDefault
  {-# INLINE term #-}

instance Category Interp where
  id = Interp $ \_ a -> return (Success a)
  {-# INLINE id #-}
  Interp f . Interp g = Interp $ \r a -> do
    b <- g r a
    case b of
        Success b' -> f r b'
        Fail -> return Fail
  {-# INLINE (.) #-}

instance ArrowTry Interp where
  fail = Interp $ \_ _ -> return Fail
  {-# INLINE fail #-}
  try (Interp f) (Interp g) (Interp h) = Interp $ \r a -> do
    b <- f r a
    case b of
      Success b' -> g r b'
      Fail -> h r a
  {-# INLINE try #-}

instance Arrow Interp where
  arr f = Interp $ \_ (a,e) -> return $ Success (f a, e)
  {-# INLINE arr #-}
  first (Interp f) = Interp $ \r ((a,b),e) -> (fmap.fmap) (\(c,e') -> ((c,b),e')) (f r (a,e))
  {-# INLINE first #-}
  second (Interp f) = Interp $ \r ((a,b),e) -> (fmap.fmap) (\(c,e') -> ((a,c),e')) (f r (b,e))
  {-# INLINE second #-}

instance ArrowChoice Interp where
  left (Interp f) = Interp $ \r (a,e) -> case a of
    Left b -> (fmap.fmap) (first Left) (f r (b,e))
    Right c -> return $ Success (Right c,e)
  {-# INLINE left #-}
  right (Interp f) = Interp $ \r (a,e) -> case a of
    Left c -> return $ Success (Left c,e)
    Right b -> (fmap.fmap) (first Right) (f r (b,e))
  {-# INLINE right #-}
  (Interp f) +++ (Interp g) = Interp $ \r (a,e) -> case a of
    Left b  -> (fmap.fmap) (first Left)  (f r (b,e))
    Right b -> (fmap.fmap) (first Right) (g r (b,e))
  {-# INLINE (+++) #-}
  Interp f ||| Interp g = Interp $ \r (a,e) -> case a of
    Left b  -> f r (b,e)
    Right b -> g r (b,e)
  {-# INLINE (|||) #-}

instance ArrowAppend Interp where
  --zeroArrow = Interp (const mempty)
  Interp f <+> Interp g = Interp $ \r x -> f r x `union` g r x
  {-# INLINE (<+>) #-}

instance ArrowApply Interp where
  app = Interp $ \r ((f,x),tenv) -> runInterp f r (x,tenv)
  {-# INLINE app #-}
        
instance Deduplicate Interp where
  dedup (Interp f) = Interp $ \r a -> dedup' $ f r a
  {-# INLINE dedup #-}

instance HasTermEnv Term Interp where
  getTermEnv = Interp $ \_ (_,e) -> return (return (e,e))
  {-# INLINE getTermEnv #-}
  putTermEnv = Interp $ \_ (e,_) -> return (return ((),e))
  {-# INLINE putTermEnv #-}

instance HasStratEnv Interp where
  readStratEnv = Interp $ \r (_,e) -> return $ Success (r,e)
  {-# INLINE readStratEnv #-}
  localStratEnv (Interp f) = Interp $ \_ ((a,r),e) -> f r (a,e)
  {-# INLINE localStratEnv #-}
