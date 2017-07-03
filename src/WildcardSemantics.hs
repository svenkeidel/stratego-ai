{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module WildcardSemantics where

import           Prelude hiding (id,fail,concat,sequence,all,(.))

import           InterpreterArrow
import           SharedSemantics
import qualified ConcreteSemantics as C
import           Syntax hiding (Fail,TermPattern(..))
import qualified Syntax as S
import           Utils

import           Control.DeepSeq
import           Control.Category
import           Control.Monad hiding (fail,sequence)
import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Append
import           Control.Arrow.Try

import           Data.Term (HasTerm(..),TermF,stringLiteral,wildcard)
import qualified Data.Term as T
import           Data.TermEnv
import           Data.Constructor
import           Data.Powerset
import qualified Data.Powerset as P
import           Data.Hashable
import           Data.Text (Text,append)
import           Data.Order
import           Data.Complete

import           Test.QuickCheck hiding (Result(..))

data Term
    = Cons Constructor [Term]
    | StringLiteral Text
    | NumberLiteral Int
    | Wildcard
    deriving (Eq)

type TermEnv = AbstractTermEnv Term

eval' :: (Eq t, Hashable t, PartOrd t c, Lattice (Complete t) c,
          ArrowChoice c, ArrowAppend c, ArrowTry c, ArrowApply c,
          Deduplicate c, HasStratEnv c, HasTerm t c, HasTermEnv (AbstractTermEnv t) t c)
      => Int -> Strat -> c t t
eval' 0 _ = proc _ ->
  fail <+> wildcard -< ()
eval' i s0 = dedup $ case s0 of
  Id -> id
  S.Fail -> fail
  Seq s1 s2 -> eval' i s2 . eval' i s1 
  GuardedChoice s1 s2 s3 -> try (eval' i s1) (eval' i s2) (eval' i s3)
  One s -> lift (one (eval' i s))
  Some s -> lift (some (eval' i s))
  All s -> lift (all (eval' i s))
  Scope xs s -> scope xs (eval' i s)
  Match f -> proc t -> match -< (f,t)
  Build f -> proc _ -> build -< f
  Let bnds body -> let_ bnds body (eval' i)
  Call f ss ps -> call f ss ps (eval' (i-1))
  Prim f _ ps -> prim f ps
-- {-# SPECIALISE eval' :: Int -> Strat -> Interp StratEnv TermEnv PowersetResult Term Term #-}

prim :: (ArrowTry p, ArrowAppend p, HasTerm t p, HasTermEnv (AbstractTermEnv t) t p)
     => StratVar -> [TermVar] -> p a t
prim f ps = proc _ -> case f of
    "SSL_strcat" -> do
      args <- lookupTermArgs -< ps
      case args of
        [T.StringLiteral t1, T.StringLiteral t2] -> stringLiteral -< t1 `append` t2
        [T.Wildcard, _] -> wildcard -< ()
        [_, T.Wildcard] -> wildcard -< ()
        _ -> fail -< ()
    "SSL_newname" -> do
      args <- lookupTermArgs -< ps
      case args of
        [T.StringLiteral _] -> wildcard -< ()
        [T.Wildcard] -> wildcard -< ()
        _ -> fail -< ()
    _ -> error ("unrecognized primitive function: " ++ show f) -< ()
  where
    lookupTermArgs = undefined
      -- proc args -> do
      -- tenv <- getTermEnv -< ()
      -- case mapM (`M.lookup` tenv) args of
      --   Just t -> mapA matchTerm -< t
      --   Nothing -> fail <+> success -< [T.Wildcard | _ <- args]
-- {-# SPECIALISE prim :: StratVar -> [TermVar] -> Interp StratEnv TermEnv PowersetResult Term Term #-}

-- Instances -----------------------------------------------------------------------------------------

matchTermDefault :: Term -> TermF Term
matchTermDefault t = case t of
  Cons c ts -> T.Cons c ts
  StringLiteral s -> T.StringLiteral s
  NumberLiteral n -> T.NumberLiteral n
  Wildcard -> T.Wildcard

termDefault :: TermF Term -> Term
termDefault t = case t of
  T.Cons c ts -> Cons c ts
  T.NumberLiteral n -> NumberLiteral n
  T.StringLiteral s -> StringLiteral s
  T.Wildcard -> Wildcard
  _ -> error "cannot construct term"

instance Monad m => HasTerm Term (Interp r s m) where
  matchTerm = arr matchTermDefault
  term = arr termDefault

instance HasTerm Term (->) where
  matchTerm = arr matchTermDefault
  term = arr termDefault

instance ArrowChoice c => PreOrd Term c where
  (⊑) = proc (t1,t2) -> case (t1,t2) of
    (_,Wildcard) -> returnA -< True
    (Cons c ts,Cons c' ts') -> do
      b <- (⊑) -< (ts,ts')
      returnA -< c == c' && b
    (StringLiteral s, StringLiteral s') -> returnA -< s == s'
    (NumberLiteral n, NumberLiteral n') -> returnA -< n == n'
    (_, _) -> returnA -< False

instance ArrowChoice c => PartOrd Term c

instance ArrowChoice c => Lattice Term c where
  (⊔) = proc (t1,t2) -> case (t1,t2) of
    (Cons c ts, Cons c' ts')
      | c == c' && eqLength ts ts' -> do
        ts'' <- zipWithA (⊔) -< (ts,ts')
        returnA -< Cons c ts''
      | otherwise -> returnA -< Wildcard
    (StringLiteral s, StringLiteral s')
      | s == s' -> returnA -< StringLiteral s
      | otherwise -> returnA -< Wildcard
    (NumberLiteral n, NumberLiteral n')
      | n == n' -> returnA -< NumberLiteral n
      | otherwise -> returnA -< Wildcard
    (Wildcard, _) -> returnA -< Wildcard
    (_, Wildcard) -> returnA -< Wildcard
    (_, _) -> returnA -< Wildcard

instance ArrowChoice p => Lattice (Complete Term) p where
  (⊔) = proc (x,y) -> case (x,y) of
    (Complete t1, Complete t2) -> Complete ^<< (⊔) -< (t1,t2)
    (_,_) -> returnA -< Top

instance ArrowChoice p => Galois (Pow C.Term) Term p where
  alpha = lub <<< P.map go
    where
      go = proc t -> case t of
        C.Cons c ts -> do
          ts' <- mapA go -< ts
          returnA -< Cons c ts'
        C.StringLiteral s -> returnA -< StringLiteral s
        C.NumberLiteral s -> returnA -< NumberLiteral s
  gamma = error "Infinite"

instance Show Term where
  show (Cons c ts) = show c ++ show ts
  show (StringLiteral s) = show s
  show (NumberLiteral n) = show n
  show Wildcard = "_"

instance Num Term where
  t1 + t2 = Cons "Add" [t1,t2]
  t1 - t2 = Cons "Sub" [t1,t2]
  t1 * t2 = Cons "Mul" [t1,t2]
  abs t = Cons "Abs" [t]
  signum t = Cons "Signum" [t]
  fromInteger = NumberLiteral . fromIntegral

instance Hashable Term where
  hashWithSalt s (Cons c ts) = s `hashWithSalt` (0::Int) `hashWithSalt` c `hashWithSalt` ts
  hashWithSalt s (StringLiteral t) = s `hashWithSalt` (1::Int) `hashWithSalt` t
  hashWithSalt s (NumberLiteral n) = s `hashWithSalt` (2::Int) `hashWithSalt` n
  hashWithSalt s Wildcard = s `hashWithSalt` (3::Int)

instance NFData Term where
  rnf t = case t of
    Cons c ts -> rnf c `seq` rnf ts
    StringLiteral s -> rnf s
    NumberLiteral n -> rnf n
    Wildcard -> ()

instance Arbitrary Term where
  arbitrary = do
    he <- choose (0,7)
    wi <- choose (0,4)
    arbitraryTerm he wi

arbitraryTerm :: Int -> Int -> Gen Term
arbitraryTerm 0 _ =
  oneof
    [ Cons <$> arbitrary <*> pure []
    , StringLiteral <$> arbitraryLetter
    , NumberLiteral <$> choose (0,9)
    , pure Wildcard
    ]
arbitraryTerm h w = do
  w' <- choose (0,w)
  c <- arbitrary
  fmap (Cons c) $ vectorOf w' $ join $
    arbitraryTerm <$> choose (0,h-1) <*> pure w
