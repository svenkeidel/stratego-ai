{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module WildcardSemantics where

import           Prelude hiding (id,fail,concat,sequence,all,zipWith,(.))
import qualified Prelude as P

import           Syntax hiding (Fail,TermPattern(..))
import           Syntax (TermPattern)
import qualified Syntax as S
import           Interpreter

import           Control.Category
import           Control.Monad hiding (fail,sequence)
import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Operations
import           Control.Arrow.Transformer.Deduplicate

import           Data.Semigroup (Semigroup(..))
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import           Data.Hashable

import           Test.QuickCheck hiding (Result(..))

data Term
    = Cons Constructor [Term]
    | StringLiteral Text
    | NumberLiteral Int
    | Wildcard
    deriving (Eq)

type TermEnv = HashMap TermVar Term

eval' :: (ArrowChoice p, ArrowState TermEnv p, ArrowAppend p, Try p, Deduplicate p, ArrowApply p)
      => Int -> StratEnv -> Strat -> p Term Term
eval' 0 _ _ = proc _ ->
  fail <+> success -< Wildcard
eval' i senv s0 = dedup $ case s0 of
  Id -> id
  S.Fail -> fail
  Seq s1 s2 -> eval' i senv s2 . eval' i senv s1 
  GuardedChoice s1 s2 s3 -> try (eval' i senv s1) (eval' i senv s2) (eval' i senv s3)
  One s -> lift (one (eval' i senv s))
  Some s -> lift (some (eval' i senv s))
  All s -> lift (all (eval' i senv s))
  Scope xs s -> scope xs (eval' i senv s)
  Match f -> proc t -> match -< (f,t)
  Build f -> proc _ -> build -< f
  Let bnds body -> let_ senv bnds body (eval' i)
  Call f ss ps -> call senv f ss ps (eval' (i-1))

match :: (ArrowChoice p, ArrowState TermEnv p, ArrowAppend p, Try p) => p (TermPattern,Term) Term
match = proc (p,t) -> case p of
  S.Var "_" -> success -< t
  S.Var x -> do
    env <- getTermEnv -< ()
    case M.lookup x env of
      Just t' -> equal -< (t,t')
      Nothing -> do
        putTermEnv -< M.insert x t env
        fail <+> success -< t
  S.Cons c ts -> case t of
    Cons c' ts'
      | c == c' && length ts == length ts' -> do
          ts'' <- zipWith match -< (ts,ts')
          success -< Cons c ts''
      | otherwise -> fail -< ()
    Wildcard -> do
      ts'' <- zipWith match -< (ts,[Wildcard | _ <- ts])
      fail <+> success -< Cons c ts''
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
    Wildcard ->
      (do
        match -< (c,  Wildcard)
        match -< (ts, Wildcard)
        success -< t)
      <+>
      (do
        match -< (ts, convertToList [])
        success -< t)
  S.StringLiteral s -> case t of
    StringLiteral s'
      | s == s' -> success -< t
      | otherwise -> fail -< ()
    Wildcard -> fail <+> success -< StringLiteral s
    _ -> fail -< ()
  S.NumberLiteral n -> case t of
    NumberLiteral n'
      | n == n' -> success -< t
      | otherwise -> fail -< ()
    Wildcard -> fail <+> success -< NumberLiteral n
    _ -> fail -< ()

equal :: (ArrowChoice p, ArrowAppend p, Try p) => p (Term,Term) Term
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
  (Wildcard, t) -> fail <+> success -< t
  (t, Wildcard) -> fail <+> success -< t
  (_,_) -> fail -< ()

build :: (ArrowChoice p, ArrowState TermEnv p, ArrowAppend p, Try p) => p TermPattern Term
build = proc p -> case p of
  S.Var x -> do
    env <- getTermEnv -< ()
    case M.lookup x env of
      Just t -> returnA -< t
      Nothing -> fail <+> success -< Wildcard
  S.Cons c ts -> do
    ts' <- mapA build -< ts
    returnA -< Cons c ts'
  S.Explode c ts -> do
    c' <- build -< c
    case c' of
      StringLiteral s -> do
        ts' <- build -< ts
        ts'' <- convertFromList -< ts'
        case ts'' of
          Just tl -> success -< Cons (Constructor s) tl
          Nothing -> fail <+> returnA -< Wildcard
      Wildcard -> fail <+> returnA -< Wildcard
      _ -> fail -< ()
  S.NumberLiteral n -> returnA -< NumberLiteral n
  S.StringLiteral s -> returnA -< StringLiteral s

convertToList :: [Term] -> Term
convertToList ts = case ts of
  (x:xs) -> Cons "Cons" [x,convertToList xs]
  [] -> Cons "Nil" []

convertFromList :: (ArrowChoice p, Try p) => p Term (Maybe [Term])
convertFromList = proc t -> case t of
  Cons "Cons" [x,tl] -> do
    xs <- convertFromList -< tl
    returnA -< (x:) <$> xs
  Cons "Nil" [] ->
    returnA -< Just []
  Wildcard -> returnA -< Nothing
  _ -> fail -< ()

lift :: (Try p,ArrowChoice p,ArrowAppend p)
     => p (Constructor,[Term]) (Constructor,[Term])
     -> p Term Term
lift p = proc t -> case t of
  Cons c ts -> do
    (c',ts') <- p -< (c,ts)
    returnA -< Cons c' ts'
  StringLiteral {} -> returnA -< t
  NumberLiteral {} -> returnA -< t
  Wildcard -> fail <+> success -< Wildcard

-- Instances -----------------------------------------------------------------------------------------

instance Semigroup Term where
  t1 <> t2 = case (t1,t2) of
    (Cons c ts, Cons c' ts')
      | c == c' && length ts == length ts' -> Cons c (P.zipWith (<>) ts ts')
      | otherwise -> Wildcard
    (StringLiteral s, StringLiteral s')
      | s == s' -> StringLiteral s
      | otherwise -> Wildcard
    (NumberLiteral n, NumberLiteral n')
      | n == n' -> NumberLiteral n
      | otherwise -> Wildcard
    (_, _) -> Wildcard

instance Monoid Term where
  mempty = undefined -- Wildcard
  mappend = undefined --(<>)

instance Show Term where
  show (Cons c ts) = show c ++ if null ts then "" else show ts
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

instance Arbitrary Term where
  arbitrary = do
    height <- choose (0,7)
    width <- choose (0,4)
    arbitraryTerm height width

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
