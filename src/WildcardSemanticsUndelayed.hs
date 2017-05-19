{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WildcardSemanticsUndelayed where

import Prelude hiding (fail)

import Interpreter
import Syntax hiding (Fail,TermPattern(..))
import Utils
import WildcardSemantics 

import Control.Arrow hiding ((<+>))
import Control.Category

import Data.Powerset
import Data.Term(HasTerm(..))
import Data.UncertainResult

eval :: Int -> StratEnv -> Strat -> (Term,TermEnv) -> UncertainResult (Term,TermEnv)
eval i senv s = runInterp (eval' i s) senv
{-# INLINE eval #-}

newtype Interp a b = Interp { runInterp :: StratEnv -> (a,TermEnv) -> UncertainResult (b,TermEnv) }

instance HasTerm Term Interp where
  matchTerm = arr matchTermDefault
  {-# INLINE matchTerm #-}
  term = arr termDefault
  {-# INLINE term #-}

instance Category Interp where
  id = Interp $ \_ a -> Success a
  {-# INLINE id #-}
  Interp f . Interp g = Interp $ \r a ->
    case g r a of
      Success b' -> f r b'
      SuccessOrFail b' -> case f r b' of
        Success c -> SuccessOrFail c
        SuccessOrFail c -> SuccessOrFail c
        Fail -> Fail
      Fail -> Fail
  {-# INLINE (.) #-}

instance ArrowTry Interp where
  fail = Interp $ \_ _ -> Fail
  {-# INLINE fail #-}
  try (Interp f) (Interp g) (Interp h) = Interp $ \r a ->
    case f r a of
      Success b -> g r b 
      SuccessOrFail b' -> g r b' `mappend` h r a
      Fail -> h r a
  {-# INLINE try #-}

instance Arrow Interp where
  arr f = Interp (\_ (a,e) -> Success (f a, e))
  {-# INLINE arr #-}
  first (Interp f) = Interp $ \r ((a,b),e) -> fmap (\(c,e') -> ((c,b),e')) (f r (a,e))
  {-# INLINE first #-}
  second (Interp f) = Interp $ \r ((a,b),e) -> fmap (\(c,e') -> ((a,c),e')) (f r (b,e))
  {-# INLINE second #-}

instance ArrowChoice Interp where
  left (Interp f) = Interp $ \r (a,e) -> case a of
    Left b -> first Left <$> f r (b,e)
    Right c -> Success (Right c,e)
  {-# INLINE left #-}
  right (Interp f) = Interp $ \r (a,e) -> case a of
    Left c -> Success (Left c,e)
    Right b -> first Right <$> f r (b,e)
  {-# INLINE right #-}
  Interp f +++ Interp g = Interp $ \r (a,e) -> case a of
    Left b  -> first Left  <$> f r (b,e)
    Right b -> first Right <$> g r (b,e)
  {-# INLINE (+++) #-}
  Interp f ||| Interp g = Interp $ \r (a,e) -> case a of
    Left b  -> f r (b,e)
    Right b -> g r (b,e)
  {-# INLINE (|||) #-}


instance ArrowAppend Interp where
  -- zeroArrow = fail
  f <+> g = Interp $ \x -> runInterp f x `mappend` runInterp g x

instance HasTermEnv Term Interp where
  getTermEnv = Interp $ \_ (_,e) -> Success (e,e)
  {-# INLINE getTermEnv #-}
  putTermEnv = Interp $ \_ (e,_) -> Success ((),e)
  {-# INLINE putTermEnv #-}

instance HasStratEnv Interp where
  readStratEnv = Interp $ \r (_,e) -> Success (r,e)
  {-# INLINE readStratEnv #-}
  localStratEnv (Interp f) = Interp $ \_ ((a,r),e) -> f r (a,e)
  {-# INLINE localStratEnv #-}

instance ArrowApply Interp where
  app = Interp $ \r ((f,b),e) -> runInterp f r (b,e)
  {-# INLINE app #-}

instance Deduplicate Interp where
  dedup f = f
  {-# INLINE dedup #-}
