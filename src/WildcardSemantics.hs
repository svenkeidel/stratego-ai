{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module WildcardSemantics where

import           Prelude hiding (id,fail,concat,sequence,all,(.))

import           InterpreterArrow
import           SharedSemantics
import           Syntax hiding (Fail,TermPattern(..))
import           Syntax (TermPattern)
import qualified Syntax as S
import           Utils

import           Control.DeepSeq
import           Control.Category
import           Control.Monad hiding (fail,sequence)
import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Append
import           Control.Arrow.Try

import           Data.Term (HasTerm(..),TermF,cons,numberLiteral,stringLiteral,wildcard)
import qualified Data.Term as T
import           Data.TermEnv
import           Data.Constructor
import           Data.Powerset
import           Data.Hashable
import           Data.Text (Text,append)
import           Data.Order

import           Test.QuickCheck hiding (Result(..))

data Term
    = Cons Constructor [Term]
    | StringLiteral Text
    | NumberLiteral Int
    | Wildcard
    deriving (Eq)

type TermEnv = AbstractTermEnv Term

eval' :: (Eq t, Hashable t, Lattice t, Lattice [t],
          ArrowChoice p, ArrowAppend p, ArrowTry p, ArrowApply p,
          Deduplicate p, HasStratEnv p, HasTerm t p, HasTermEnv (AbstractTermEnv t) t p)
      => Int -> Strat -> p t t
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

match :: (ArrowChoice p, ArrowAppend p, ArrowTry p, HasTerm t p, HasTermEnv env t p, Lattice t)
      => p (TermPattern,t) t
match = proc (p,t) -> case p of
  S.As v p2 -> do
    t' <- match -< (S.Var v,t)
    match -< (p2,t')
  S.Var "_" -> success -< t
  S.Var x -> do
    m <- lookupTermVar -< x
    case m of
      Just t' -> do
        t'' <- equal -< (t,t')
        insertTerm -< (x,t'')
        success -< t''
      Nothing -> do
        insertTerm -< (x,t)
        returnA -< t
  S.Cons c ts -> do
    m <- matchTermAgainstConstructor -< (c,t)
    case m of
      T.Cons c' ts'
        | eqLength ts ts' -> do
            ts'' <- zipWithA match -< (ts,ts')
            T.cons -< (c',ts'')
        | otherwise -> fail -< ()
      T.Wildcard -> do
        l <- mapA wildcard -< [() | _ <- ts]
        ts'' <- zipWithA match -< (ts,l)
        fail <+> cons -< (c,ts'')
      _ -> fail -< ()
  S.Explode c ts -> do
    m <- matchTerm -< t
    case m of
      T.Cons (Constructor c') ts' -> do
        s <- stringLiteral -< c'
        match -< (c,s)
        l <- convertToList -< ts'
        match -< (ts, l)
        success -< t
      T.StringLiteral _ -> do
        l <- convertToList -< []
        match -< (ts, l)
        success -< t
      T.NumberLiteral _ -> do
        l <- convertToList -< []
        match -< (ts, l)
        success -< t
      T.Wildcard ->
        (do
          w <- wildcard -< ()
          match -< (c,  w)
          w' <- wildcard -< ()
          match -< (ts, w')
          success -< t)
        <+>
        (do
          l <- convertToList -< []
          match -< (ts, l)
          success -< t)
      _ -> fail -< ()
  S.StringLiteral s -> do
    m <- matchTerm -< t
    case m of
      T.StringLiteral s'
        | s == s' -> success -< t
        | otherwise -> fail -< ()
      T.Wildcard -> fail <+> stringLiteral -< s
      _ -> fail -< ()
  S.NumberLiteral n -> do
    m <- matchTerm -< t
    case m of
      T.NumberLiteral n'
        | n == n' -> success -< t
        | otherwise -> fail -< ()
      T.Wildcard -> fail <+> numberLiteral -< n
      _ -> fail -< ()
-- {-# SPECIALISE match :: Interp StratEnv TermEnv PowersetResult (TermPattern, Term) Term #-}

build :: (ArrowChoice p, ArrowAppend p, ArrowTry p, HasTerm t p, HasTermEnv env t p, Lattice t)
      => p TermPattern t
build = proc p -> case p of
  S.As _ _ -> error "As-pattern in build is disallowed" -< ()
  S.Var x -> do
    m <- lookupTermVar -< x
    case m of
      Just t -> returnA -< t
      Nothing -> wildcard -< ()
  S.Cons c ts -> do
    ts' <- mapA build -< ts
    cons -< (c,ts')
  S.Explode c ts -> do
    m <- matchTerm <<< build -< c
    case m of
      T.StringLiteral s -> do
        ts' <- build -< ts
        ts'' <- convertFromList -< ts'
        case ts'' of
          Just tl -> cons -< (Constructor s,tl)
          Nothing -> fail <+> wildcard -< ()
      T.Wildcard -> fail <+> wildcard -< ()
      _ -> fail -< ()
  S.NumberLiteral n -> numberLiteral -< n
  S.StringLiteral s -> stringLiteral -< s
-- {-# SPECIALISE build :: Interp StratEnv TermEnv PowersetResult TermPattern Term #-}

-- Instances -----------------------------------------------------------------------------------------

matchTermDefault :: Term -> TermF Term
matchTermDefault t = case t of
  Cons c ts -> T.Cons c ts
  StringLiteral s -> T.StringLiteral s
  NumberLiteral n -> T.NumberLiteral n
  Wildcard -> T.Wildcard
{-# INLINE matchTermDefault #-}

termDefault :: TermF Term -> Term
termDefault t = case t of
  T.Cons c ts -> Cons c ts
  T.NumberLiteral n -> NumberLiteral n
  T.StringLiteral s -> StringLiteral s
  T.Wildcard -> Wildcard
  _ -> error "cannot construct term"
{-# INLINE termDefault #-}

instance Monad m => HasTerm Term (Interp r s m) where
  matchTerm = arr matchTermDefault
  {-# INLINE matchTerm #-}
  term = arr termDefault
  {-# INLINE term #-}

instance HasTerm Term (->) where
  matchTerm = arr matchTermDefault
  {-# INLINE matchTerm #-}
  term = arr termDefault
  {-# INLINE term #-}

instance PreOrd Term where
  _ ⊑ Wildcard = True
  Cons c ts ⊑ Cons c' ts' = c == c' && ts ⊑ ts'
  StringLiteral s ⊑ StringLiteral s' = s == s'
  NumberLiteral n ⊑ NumberLiteral n' = n == n'
  _ ⊑ _ = False

instance PartOrd Term

instance Lattice Term where
  Cons c ts ⊔ Cons c' ts'
    | c == c' && eqLength ts ts' = Cons c (zipWith (⊔) ts ts')
    | otherwise = Wildcard
  StringLiteral s ⊔ StringLiteral s'
    | s == s' = StringLiteral s
    | otherwise = Wildcard
  NumberLiteral n ⊔ NumberLiteral n'
    | n == n' = NumberLiteral n
    | otherwise = Wildcard
  Wildcard ⊔ _ = Wildcard
  _ ⊔ Wildcard = Wildcard
  _ ⊔ _ = Wildcard

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
