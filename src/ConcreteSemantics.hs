{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module ConcreteSemantics where

import           Prelude hiding (id,(.),fail,all)

import           InterpreterArrow
import           SharedSemantics
import           Syntax (TermPattern)
import qualified Syntax as S
import           Syntax hiding (Fail,TermPattern(..))
import           Utils

import           Control.Arrow
import           Control.Arrow.Try
import           Control.Arrow.Append
import           Control.Category
import           Control.Monad hiding (fail)

import           Data.Constructor
import           Data.Hashable
import           Data.Result
import           Data.String (IsString(..))
import           Data.Term (HasTerm(..),TermF)
import           Data.TermEnv
import qualified Data.Term as T
import           Data.Text (Text)
-- import qualified Data.Text as Text
import           Data.Order
import           Data.Complete

import           Test.QuickCheck hiding (Result(..))

data Term
  = Cons Constructor [Term]
  | StringLiteral Text
  | NumberLiteral Int
  deriving (Eq)

type TermEnv = ConcreteTermEnv Term

eval :: StratEnv -> Strat -> (Term,TermEnv) -> Result (Term,TermEnv)
eval senv s = runInterp (eval' s) senv
{-# INLINE eval #-}

eval' :: (ArrowChoice c, ArrowTry c, ArrowAppend c, ArrowApply c, HasTerm t c, HasTermEnv env t c, HasStratEnv c, PartOrd t c, Lattice (Complete t) c) => Strat -> c t t
eval' s0 = case s0 of
  Id -> id
  S.Fail -> fail
  Seq s1 s2 -> eval' s2 . eval' s1
  GuardedChoice s1 s2 s3 -> try (eval' s1) (eval' s2) (eval' s3)
  One s -> lift (one (eval' s))
  Some s -> lift (some (eval' s))
  All s -> lift (all (eval' s))
  Scope xs s -> scope xs (eval' s)
  Match f -> proc t -> match -< (f,t)
  Build f -> proc _ -> build -< f
  Let bnds body -> let_ bnds body eval'
  Call f ss ps -> call f ss ps eval'
  Prim f _ ps -> prim f ps

prim :: (ArrowTry p, HasTerm t p, HasTermEnv env t p) => StratVar -> [TermVar] -> p t t
prim = undefined
-- prim f ps = proc _ -> case f of
--   "strcat" -> do
--     tenv <- getTermEnv -< ()
--     case mapM (`M.lookup` tenv) ps of
--       Just [t1, t2] -> do
--         m <- matchTerm *** matchTerm -< (t1,t2)
--         case m of
--           (T.StringLiteral s1,T.StringLiteral s2) ->
--             T.stringLiteral -< s1 `Text.append` s2
--           _ -> fail -< ()
--       _ -> fail -< ()
--   "SSL_newname" -> do
--     tenv <- getTermEnv -< ()
--     case mapM (`M.lookup` tenv) ps of
--       Just [_] -> undefined -< ()
--       _ -> fail -< ()
--   _ -> error ("unrecognized primitive function: " ++ show f) -< ()

-- match :: (ArrowTry p, ArrowChoice p, ArrowAppend p, HasTerm t p, HasTermEnv env t p, Lattice (Complete t) p) => p (TermPattern, t) t
-- match = proc (p,t) -> case p of
--   S.As v p2 -> do
--     t' <- match -< (S.Var v,t)
--     match -< (p2,t')
--   S.Var "_" -> success -< t
--   S.Var x -> do
--     m <- lookupTermVar -< x
--     case m of
--       Just t' -> do
--         t'' <- equal -< (t,t')
--         insertTerm -< (x,t'')
--         success -< t''
--       Nothing -> do
--         insertTerm -< (x,t)
--         returnA -< t
--   S.Cons c ts -> do
--     m <- matchTermAgainstConstructor -< (c, t)
--     case m of
--       T.Cons _ ts'
--         | eqLength ts ts' -> do
--             ts'' <- zipWithA match -< (ts,ts')
--             T.cons -< (c,ts'')
--         | otherwise -> fail -< ()
--       _ -> fail -< ()
--   S.Explode c ts -> do
--     m <- matchTermRefine -< t
--     case m of
--       T.Cons (Constructor c') ts' -> do
--         s <- T.stringLiteral -< c'
--         match -< (c,s)
--         l <- convertToList -< ts'
--         match -< (ts, l)
--         success -< t
--       T.StringLiteral _ -> do
--         l <- convertToList -< []
--         match -< (ts, l)
--         success -< t
--       T.NumberLiteral _ -> do
--         l <- convertToList -< []
--         match -< (ts,l) 
--         success -< t
--       _ -> fail -< ()
--   S.StringLiteral s -> do
--     m <- matchTerm -< t
--     case m of
--       T.StringLiteral s'
--         | s == s' -> success -< t
--         | otherwise -> fail -< ()
--       _ -> fail -< ()
--   S.NumberLiteral n -> do
--     m <- matchTerm -< t
--     case m of
--       T.NumberLiteral n'
--         | n == n' -> success -< t
--         | otherwise -> fail -< ()
--       _ -> fail -< ()

-- build :: (ArrowChoice p, ArrowTry p, HasTerm t p, HasTermEnv env t p) => p TermPattern t
-- build = proc p -> case p of
--   S.As _ _ -> error "As-pattern in build is disallowed" -< ()
--   S.Var x -> do
--     m <- lookupTermVar -< x
--     case m of
--       Just t -> returnA -< t
--       Nothing -> T.wildcard -< ()
--     -- env <- getTermEnv -< ()
--     -- case M.lookup x env of
--     --   Just t -> returnA -< t
--     --   Nothing -> fail -< ()
--   S.Cons c ts -> do
--     ts' <- mapA build -< ts
--     T.cons -< (c,ts')
--   S.Explode c ts -> do
--     m <- matchTerm <<< build -< c
--     case m of
--       T.StringLiteral s -> do
--         ts' <- build -< ts
--         ts'' <- arr fromJust <<< convertFromList -< ts'
--         T.cons -< (Constructor s,ts'')
--       _ -> fail -< ()
--   S.NumberLiteral n -> T.numberLiteral -< n
--   S.StringLiteral s -> T.stringLiteral -< s

-- Instances -----------------------------------------------------------------------------------------

matchTermDefault :: Term -> TermF Term
matchTermDefault t = case t of
  Cons c ts -> T.Cons c ts
  StringLiteral s -> T.StringLiteral s
  NumberLiteral n -> T.NumberLiteral n

instance (ArrowChoice c, ArrowTry c) => HasTerm Term c where
  matchTerm = arr matchTermDefault
  term = proc t -> case t of
    T.Cons c ts -> returnA -< Cons c ts
    T.NumberLiteral n -> returnA -< NumberLiteral n
    T.StringLiteral s -> returnA -< StringLiteral s
    _ -> returnA -< error "cannot construct term"

instance ArrowChoice p => PreOrd Term p where
  (⊑) = proc (t1,t2) -> case (t1,t2) of
    (Cons c ts,Cons c' ts') -> do
      b <- (⊑) -< (ts,ts')
      returnA -< c == c' && b
    (StringLiteral s, StringLiteral s') -> returnA -< s == s'
    (NumberLiteral n, NumberLiteral n') -> returnA -< n == n'
    (_, _) -> returnA -< False

instance ArrowChoice p => PartOrd Term p

instance ArrowChoice p => Lattice (Complete Term) p where
  (⊔) = proc (t1,t2) -> case (t1,t2) of
    (Complete (Cons c ts), Complete (Cons c' ts'))
      | c == c' -> do
        ts'' <- zipWithA (⊔) -< (Complete <$> ts,Complete <$> ts')
        returnA -< Cons c <$> sequenceA ts''
      | otherwise -> returnA -< Top
    (Complete (StringLiteral s), Complete (StringLiteral s'))
      | s == s' -> returnA -< Complete (StringLiteral s)
      | otherwise -> returnA -< Top
    (Complete (NumberLiteral n), Complete (NumberLiteral n'))
      | n == n' -> returnA -< Complete (NumberLiteral n)
      | otherwise -> returnA -< Top
    (_, _) -> returnA -< Top

instance Show Term where
  show (Cons c ts) = show c ++ if null ts then "" else show ts
  show (StringLiteral s) = show s
  show (NumberLiteral n) = show n

instance IsString Term where
  fromString = StringLiteral . fromString

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

instance Arbitrary Term where
  arbitrary = do
    h <- choose (0,7)
    w <- choose (0,4)
    arbitraryTerm h w

height :: Term -> Int
height t = case t of
  Cons _ ts -> maximum (fmap height ts ++ [0]) + 1
  _ -> 1

similar :: Gen (Term,Term)
similar = do
  [t1,t2] <- similarTerms 2 5 2 7
  return (t1,t2)

similarTerms :: Int -> Int -> Int -> Int -> Gen [Term]
similarTerms m h w similarity = do
  t <- arbitraryTerm h w
  replicateM m (go 1 t)
  where
    go :: Int -> Term -> Gen Term
    go i t = distribution (i,height t + similarity) (arbitraryTerm h w) $ case t of
       Cons c ts -> Cons c <$> traverse (go (i+1)) ts
       _ -> return t

similarTermPattern :: Term -> Int -> Gen TermPattern
similarTermPattern t0 similarity = go 0 t0
  where
    go :: Int -> Term -> Gen TermPattern
    go i t = distribution (i,height t + similarity) (S.Var <$> arbitrary) $ case t of
       Cons c ts -> S.Cons c <$> traverse (go (i+1)) ts
       StringLiteral s -> return $ S.StringLiteral s
       NumberLiteral n -> return $ S.NumberLiteral n

distribution :: (Int,Int) -> Gen a -> Gen a -> Gen a
distribution (p,n) a b = oneof (replicate p a ++ replicate (n-p) b)

arbitraryTerm :: Int -> Int -> Gen Term
arbitraryTerm 0 _ =
  oneof
    [ Cons <$> arbitrary <*> pure []
    , StringLiteral <$> arbitraryLetter
    , NumberLiteral <$> choose (0,9)
    ]
arbitraryTerm h w = do
  w' <- choose (0,w)
  c <- arbitrary
  fmap (Cons c) $ vectorOf w' $ join $
    arbitraryTerm <$> choose (0,h-1) <*> pure w

