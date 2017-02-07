{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module ConcreteSemantics where

import Prelude hiding (id,(.),fail,sequence,all,uncurry,zipWith)

import Syntax hiding (Fail,TermPattern(..))
import Syntax (TermPattern)
import qualified Syntax as S
import Interpreter
import Result

import Control.Monad hiding (fail,sequence)
import Control.Category
import Control.Arrow

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)

import Test.QuickCheck hiding (Result(..))

data Term
  = Cons Constructor [Term]
  | StringLiteral Text
  | NumberLiteral Int
  deriving Eq

type TermEnv = Map TermVar Term
newtype Interp a b = Interp { runInterp :: StratEnv -> (a,TermEnv) -> Result (b,TermEnv) }

eval :: Strat -> StratEnv -> (Term,TermEnv) -> Result (Term,TermEnv)
eval s = runInterp (interp s)

interp :: Strat -> Interp Term Term
interp s0 = case s0 of
  S.Fail -> fail
  Id -> id
  Seq s1 s2 -> sequence (interp s1) (interp s2)
  GuardedChoice s1 s2 s3 -> guardedChoice (interp s1) (interp s2) (interp s3)
  One s -> lift (one (interp s))
  Some s -> lift (some (interp s))
  All s -> lift (all (interp s))
  Scope xs s -> scope xs (interp s)
  Match f -> proc t -> match -< (f,t)
  Build f -> proc _ -> build -< f
  Let ss body -> let_ ss (interp body)
  Call f ss ps -> call f ss ps interp

match :: (ArrowChoice p, Try p, HasTermEnv TermEnv p) => p (TermPattern, Term) Term
match = proc (p,t) -> case p of
  S.Var "_" -> success -< t
  S.Var x -> do
    env <- getTermEnv -< ()
    case M.lookup x env of
      Just t' -> unify -< (t,t')
      Nothing -> do
        putTermEnv -< M.insert x t env
        success -< t
  S.Cons c ts -> case t of
    Cons c' ts'
      | c == c' && length ts == length ts' -> do
          ts'' <- zipWith match -< (ts,ts')
          success -< Cons c ts''
      | otherwise -> fail -< ()
    _ -> fail -< ()
  S.Explode c ts -> case t of
    Cons (Constructor c') ts' -> do
      _ <- match -< (c,StringLiteral c')
      _ <- match -< (ts, convertToList ts')
      success -< t
    _ -> fail -< ()
  S.StringLiteral s -> case t of
    StringLiteral s'
      | s == s' -> success -< t
      | otherwise -> fail -< ()
    _ -> fail -< ()
  S.NumberLiteral n -> case t of
    NumberLiteral n'
      | n == n' -> success -< t
      | otherwise -> fail -< ()
    _ -> fail -< ()

unify :: (ArrowChoice p, Try p) => p (Term,Term) Term
unify = proc (t1,t2) -> case (t1,t2) of
  (Cons c ts,Cons c' ts')
    | c == c' && length ts == length ts' -> do
      ts'' <- zipWith unify -< (ts,ts')
      returnA -< Cons c ts''
    | otherwise -> fail -< ()
  (StringLiteral s, StringLiteral s')
    | s == s' -> success -< t1
    | otherwise -> fail -< ()
  (NumberLiteral n, NumberLiteral n')
    | n == n' -> success -< t1
    | otherwise -> fail -< ()
  (_,_) -> fail -< ()


build :: (ArrowChoice p, Try p, HasTermEnv (Map TermVar Term) p) => p TermPattern Term
build = proc p -> case p of
  S.Var x -> do
    env <- getTermEnv -< ()
    case M.lookup x env of
      Nothing -> fail -< ()
      Just t -> success -< t
  S.Cons c ts -> do
    ts' <- mapA build -< ts
    returnA -< Cons c ts'
  S.Explode c ts -> do
    c' <- build -< c
    case c' of
      StringLiteral s -> do
        ts' <- build -< ts
        ts'' <- convertFromList -< ts'
        returnA -< Cons (Constructor s) ts''
      _ -> fail -< ()
  S.NumberLiteral n -> returnA -< NumberLiteral n
  S.StringLiteral s -> returnA -< StringLiteral s

convertToList :: [Term] -> Term
convertToList ts = case ts of
  (x:xs) -> Cons "Cons" [x,convertToList xs]
  [] -> Cons "Nil" []

convertFromList :: (ArrowChoice p, Try p) => p Term [Term]
convertFromList = proc t -> case t of
  Cons "Cons" [x,tl] -> do
    xs <- convertFromList -< tl
    returnA -< x:xs
  Cons "Nil" [] ->
    returnA -< []
  _ -> fail -< ()

lift :: ArrowChoice p => p (Constructor,[Term]) (Constructor,[Term]) -> p Term Term
lift p = proc t -> case t of
  Cons c ts -> do
    (c',ts') <- p -< (c,ts)
    returnA -< Cons c' ts'
  StringLiteral {} -> returnA -< t
  NumberLiteral {} -> returnA -< t

-- Instances -----------------------------------------------------------------------------------------

instance Category Interp where
  id = Interp (const Success)
  f . g = Interp $ \senv -> runInterp g senv >=> runInterp f senv

instance Arrow Interp where
  arr f = Interp (\_ (a,e) -> Success (f a, e))
  first f = Interp $ \senv ((a,b),e) -> fmap (\(c,e') -> ((c,b),e')) (runInterp f senv (a,e))
  second f = Interp $ \senv ((a,b),e) -> fmap (\(c,e') -> ((a,c),e')) (runInterp f senv (b,e))

instance ArrowChoice Interp where
  left f = Interp $ \senv (a,e) -> case a of
    Left b -> fmap (first Left) (runInterp f senv (b,e))
    Right c -> Success (Right c,e)
  right f = Interp $ \senv (a,e) -> case a of
    Left c -> Success (Left c,e)
    Right b -> fmap (first Right) (runInterp f senv (b,e))
  f +++ g = Interp $ \senv (a,e) -> case a of
    Left b  -> fmap (first Left)  (runInterp f senv (b,e))
    Right b -> fmap (first Right) (runInterp g senv (b,e))

instance Try Interp where
  success = Interp (const Success)
  fail = Interp (const (const Fail))
  try t s f = Interp $ \senv (a,e) -> case runInterp t senv (a,e) of
    Success (b,e') -> runInterp s senv (b,e')
    Fail -> runInterp f senv (a,e)

instance ArrowZero Interp where
  zeroArrow = fail

instance ArrowPlus Interp where
  f <+> g = Interp $ \senv x -> case (runInterp f senv x,runInterp g senv x) of
    (Success y,_) -> Success y
    (_,Success y) -> Success y
    (_,_) -> Fail

instance ArrowApply Interp where
  app = Interp $ \senv ((f,b),e) -> runInterp f senv (b,e)

instance HasTermEnv (Map TermVar Term) Interp where
  getTermEnv = Interp $ \_ ((),e) -> Success (e,e)
  putTermEnv = Interp $ \_ (e,_) -> Success ((),e)

instance HasStratEnv Interp where
  readStratEnv = Interp $ \senv (_,e) -> return (senv,e)
  localStratEnv f = Interp $ \_ ((a,senv'),e) -> runInterp f senv' (a,e)

instance Show Term where
  show (Cons c ts) = show c ++ if null ts then "" else show ts
  show (StringLiteral s) = show s
  show (NumberLiteral n) = show n

instance Num Term where
  t1 + t2 = Cons "Add" [t1,t2]
  t1 - t2 = Cons "Sub" [t1,t2]
  t1 * t2 = Cons "Mul" [t1,t2]
  abs t = Cons "Abs" [t]
  signum t = Cons "Signum" [t]
  fromInteger = NumberLiteral . fromIntegral

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

