{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module ConcreteSemantics where

import           Prelude hiding (id,(.),fail,all,curry,uncurry)

import           InterpreterArrow
import           SharedSemantics
import           Syntax (TermPattern)
import qualified Syntax as S
import           Syntax hiding (Fail,TermPattern(..))
import           Utils

import           Control.Arrow
import           Control.Arrow.Try
import           Control.Arrow.Join
import           Control.Arrow.Apply
import           Control.Category
import           Control.Monad hiding (fail)

import           Data.Constructor
import           Data.Hashable
import           Data.Result
import           Data.String (IsString(..))
import           Data.Term (IsTerm(..),TermUtils(..))
import           Data.TermEnv
import           Data.Text (Text)
import           Data.Order
import           Data.Complete

import           Test.QuickCheck hiding (Result(..))

data Term
  = Cons Constructor [Term]
  | StringLiteral Text
  | NumberLiteral Int
  deriving (Eq)

type TermEnv = ConcreteTermEnv Term

eval'' :: Strat -> Interp StratEnv TermEnv Result Term Term
eval'' = eval' (-1)

eval :: StratEnv -> Strat -> (Term,TermEnv) -> Result (Term,TermEnv)
eval senv s = runInterp (eval' (-1) s) senv
-- eval senv s = runInterp (eval' 0 0 $$ s) senv

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

-- Instances -----------------------------------------------------------------------------------------

instance (ArrowChoice c, ArrowTry c, ArrowJoin c) => IsTerm Term c where
  matchTermAgainstConstructor matchSubterms = proc (c,ts,t) -> case t of
    Cons c' ts'
      | c == c' && eqLength ts ts' -> do
        ts'' <- matchSubterms -< (ts,ts')
        returnA -< Cons c ts''
      | otherwise ->
        fail -< ()
    _ -> fail -< ()

  matchTermAgainstString = proc (s,t) -> case t of
    StringLiteral s'
      | s == s' -> returnA -< t
      | otherwise -> fail -< ()
    _ -> fail -< ()

  matchTermAgainstNumber = proc (n,t) -> case t of
    NumberLiteral n'
      | n == n' -> returnA -< t
      | otherwise -> fail -< ()
    _ -> fail -< ()

  matchTermAgainstExplode matchCons matchSubterms = proc t -> case t of
      Cons (Constructor c) ts -> do
        matchCons -< (StringLiteral c)
        matchSubterms -< convertToList ts
        returnA -< t
      StringLiteral _ -> do
        matchSubterms -< convertToList []
        returnA -< t
      NumberLiteral _ -> do
        matchSubterms -< convertToList []
        returnA -< t
  
  equal = proc (t1,t2) ->
    case (t1,t2) of
      (Cons c ts, Cons c' ts')
          | c == c' && eqLength ts ts' -> do
          ts'' <- zipWithA equal -< (ts,ts')
          cons -< (c,ts'')
          | otherwise -> fail -< ()
      (StringLiteral s, StringLiteral s')
          | s == s' -> success -< t1
          | otherwise -> fail -< ()
      (NumberLiteral n, NumberLiteral n')
          | n == n' -> success -< t1
          | otherwise -> fail -< ()
      (_,_) -> fail -< ()

  convertFromList = proc (c,ts) -> case (c,go ts) of
    (StringLiteral c', Just ts') -> returnA -< Cons (Constructor c') ts'
    _                            -> fail -< ()
    where
      go t = case t of
        Cons "Cons" [x,tl] -> (x:) <$> go tl
        Cons "Nil" [] -> Just []
        _ -> Nothing

  lift f = proc t ->
    case t of
      Cons c ts -> do
        ts' <- f -< ts
        cons -< (c,ts')
      StringLiteral {} -> returnA -< t
      NumberLiteral {} -> returnA -< t

  cons = arr (uncurry Cons)
  numberLiteral = arr NumberLiteral
  stringLiteral = arr StringLiteral

instance PreOrd Term where
  t1 ⊑ t2 = case (t1,t2) of
    (Cons c ts,Cons c' ts') -> c == c' && ts ⊑ ts'
    (StringLiteral s, StringLiteral s') -> s == s'
    (NumberLiteral n, NumberLiteral n') -> n == n'
    (_, _) -> False

instance PartOrd Term

instance Lattice (Complete Term) where
  t1 ⊔ t2 = case (t1,t2) of
    (Complete (Cons c ts), Complete (Cons c' ts'))
      | c == c' ->
        Cons c <$> (Complete ts ⊔ Complete ts')
      | otherwise -> Top
    (Complete (StringLiteral s), Complete (StringLiteral s'))
      | s == s' -> Complete (StringLiteral s)
      | otherwise -> Top
    (Complete (NumberLiteral n), Complete (NumberLiteral n'))
      | n == n' -> Complete (NumberLiteral n)
      | otherwise -> Top
    (_, _) -> Top

instance BoundedLattice (Complete Term) where
  top = Top

instance TermUtils Term where
  convertToList ts = case ts of
    (x:xs) -> Cons "Cons" [x,convertToList xs]
    []     -> Cons "Nil"  []
    
  size (Cons _ ts) = sum (size <$> ts) + 1
  size (StringLiteral _) = 1
  size (NumberLiteral _) = 1

  height (Cons _ []) = 1
  height (Cons _ ts) = maximum (height <$> ts) + 1
  height (StringLiteral _) = 1
  height (NumberLiteral _) = 1
  
instance Lattice Term where
  (⊔) = undefined

instance BoundedLattice Term where
  top = undefined

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

