{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module ConcreteSemantics where

import           Prelude hiding (id,(.),fail,sequence,all,uncurry,zipWith)

import           Syntax hiding (Fail,TermPattern(..))
import           Syntax (TermPattern)
import qualified Syntax as S
import           Interpreter
import           Result

import           Control.Monad hiding (fail,sequence)
import           Control.Category
import           Control.Arrow
import           Control.Arrow.Operations

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import           Data.String (IsString(..))
import           Data.Hashable
import           Data.Semigroup(Semigroup(..))

import           Test.QuickCheck hiding (Result(..))

data Term
  = Cons Constructor [Term]
  | StringLiteral Text
  | NumberLiteral Int
  deriving (Eq)

type TermEnv = HashMap TermVar Term
newtype Interp a b = Interp { runInterp :: (a,TermEnv) -> Result (b,TermEnv) }

eval :: StratEnv -> Strat -> (Term,TermEnv) -> Result (Term,TermEnv)
eval senv s = runInterp $ eval' senv s

eval' :: StratEnv -> Strat -> Interp Term Term
eval' senv s0 = case s0 of
  Id -> id
  S.Fail -> fail
  Seq s1 s2 -> eval' senv s2 . eval' senv s1
  GuardedChoice s1 s2 s3 -> try (eval' senv s1) (eval' senv s2) (eval' senv s3)
  One s -> lift (one (eval' senv s))
  Some s -> lift (some (eval' senv s))
  All s -> lift (all (eval' senv s))
  Scope xs s -> scope xs (eval' senv s)
  Match f -> proc t -> match -< (f,t)
  Build f -> proc _ -> build -< f
  Let bnds body -> let_ senv bnds body eval'
  Call f ss ps -> call senv f ss ps eval'

match :: Interp (TermPattern, Term) Term
match = proc (p,t) -> case p of
  S.Var "_" -> success -< t
  S.Var x -> do
    env <- getTermEnv -< ()
    case M.lookup x env of
      Just t' -> equal -< (t,t')
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
      match -< (c,StringLiteral c')
      match -< (ts, convertToList ts')
      success -< t
    StringLiteral _ -> do
      match -< (ts, convertToList [])
      success -< t
    NumberLiteral _ -> do
      match -< (ts, convertToList [])
      success -< t
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

equal :: (ArrowChoice p, Try p) => p (Term,Term) Term
equal = proc (t1,t2) -> case (t1,t2) of
  (Cons c ts,Cons c' ts')
    | c == c' && length ts == length ts' -> do
      ts'' <- zipWith equal -< (ts,ts')
      returnA -< Cons c ts''
    | otherwise -> fail -< ()
  (StringLiteral s, StringLiteral s')
    | s == s' -> success -< t1
    | otherwise -> fail -< ()
  (NumberLiteral n, NumberLiteral n')
    | n == n' -> success -< t1
    | otherwise -> fail -< ()
  (_,_) -> fail -< ()


build :: Interp TermPattern Term
build = proc p -> case p of
  S.Var x -> do
    env <- getTermEnv -< ()
    case M.lookup x env of
      Just t -> returnA -< t
      Nothing -> fail -< ()
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
  id = Interp $ \a -> Success a
  Interp f . Interp g = Interp $ \a ->
    case g a of
      Success b -> f b
      Fail -> Fail

instance Try Interp where
  fail = Interp $ \_ -> Fail
  try (Interp f) (Interp g) (Interp h) = Interp $ \a ->
    case f a of
      Success b -> g b 
      Fail -> h a

instance Arrow Interp where
  arr f = Interp (\(a,e) -> Success (f a, e))
  first (Interp f) = Interp $ \((a,b),e) -> fmap (\(c,e') -> ((c,b),e')) (f (a,e))
  second (Interp f) = Interp $ \((a,b),e) -> fmap (\(c,e') -> ((a,c),e')) (f (b,e))

instance ArrowChoice Interp where
  left f = Interp $ \(a,e) -> case a of
    Left b -> first Left <$> runInterp f (b,e)
    Right c -> Success (Right c,e)
  right f = Interp $ \(a,e) -> case a of
    Left c -> Success (Left c,e)
    Right b -> first Right <$> runInterp f (b,e)
  f +++ g = Interp $ \(a,e) -> case a of
    Left b  -> first Left  <$> runInterp f (b,e)
    Right b -> first Right <$> runInterp g (b,e)

instance ArrowAlternative Interp where
  -- zeroArrow = fail
  f <+> g = Interp $ \x -> case (runInterp f x,runInterp g x) of
    (Success y,_) -> Success y
    (_,Success y) -> Success y
    (_,_) -> Fail

instance ArrowState TermEnv Interp where
  fetch = Interp $ \(_,e) -> Success (e,e)
  store = Interp $ \(e,_) -> Success ((),e)

instance ArrowApply Interp where
  app = Interp $ \((f,b),e) -> runInterp f (b,e)

instance Semigroup Term where
  (<>) = undefined

instance Monoid Term where
  mempty = undefined
  mappend = undefined

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

