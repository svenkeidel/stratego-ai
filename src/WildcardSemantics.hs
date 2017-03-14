{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module WildcardSemantics where

import           Prelude hiding (id,fail,concat,sequence,all,zipWith,(.))

import           Result
import           Syntax hiding (Fail,TermPattern(..))
import           Syntax (TermPattern)
import qualified Syntax as S
import           Interpreter

import           Control.Arrow
import           Control.Category
import           Control.Monad hiding (fail,sequence)

import           Data.Semigroup ((<>))
import           Data.Sequence (Seq,(|>))
import qualified Data.Sequence as S
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Text (Text)
import           Data.Hashable
import           Data.Foldable (foldl')

import           Test.QuickCheck hiding (Result(..))

data Term
    = Cons Constructor [Term]
    | StringLiteral Text
    | NumberLiteral Int
    | Wildcard
    deriving (Eq)

type TermEnv = HashMap TermVar Term
type Pow = Seq

newtype AbsInterp a b = Interp {runInterp :: (a,TermEnv) -> Pow (Result (b,TermEnv))}

eval :: Int -> StratEnv -> Strat -> (Term,TermEnv) -> Pow (Result (Term,TermEnv))
eval i senv s = runInterp (eval' i senv s)

eval' :: Int -> StratEnv -> Strat -> AbsInterp Term Term
eval' 0 senv s = proc _ -> do
  -- approximateTermEnv H.empty s -< ()
  fail <+> success -< Wildcard
eval' i senv s0 = dedup $ case s0 of
  Id -> id
  S.Fail -> fail
  Seq s1 s2 -> sequence (eval' i senv s1) (eval' i senv s2)
  GuardedChoice s1 s2 s3 -> guardedChoice (eval' i senv s1) (eval' i senv s2) (eval' i senv s3)
  One s -> lift (one (eval' i senv s))
  Some s -> lift (some (eval' i senv s))
  All s -> lift (all (eval' i senv s))
  Scope xs s -> scope xs (eval' i senv s)
  Match f -> proc t -> match -< (f,t)
  Build f -> proc _ -> build -< f
  Let bnds body -> let_ senv bnds body (eval' i)
  Call f ss ps -> call senv f ss ps (eval' (i-1))

dedup :: (Hashable b,Eq b) => AbsInterp a b -> AbsInterp a b
dedup f = Interp $ \x -> dedup' $ runInterp f x

dedup' :: (Hashable a,Eq a) => Seq a -> Seq a
dedup' = foldl' (|>) S.empty
       . foldl' (flip H.insert) H.empty

match :: AbsInterp (TermPattern,Term) Term
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
    Wildcard -> do
      ts'' <- fail <+> zipWith match -< (ts,[Wildcard | _ <- ts])
      success -< Cons c ts''
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

equal :: AbsInterp (Term,Term) Term
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

build :: (ArrowPlus p, ArrowChoice p, Try p, HasTermEnv TermEnv p) => p TermPattern Term
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

lift :: (Try p,ArrowChoice p,ArrowPlus p)
     => p (Constructor,[Term]) (Constructor,[Term])
     -> p Term Term
lift p = proc t -> case t of
  Cons c ts -> do
    (c',ts') <- p -< (c,ts)
    returnA -< Cons c' ts'
  StringLiteral {} -> returnA -< t
  NumberLiteral {} -> returnA -< t
  Wildcard -> fail <+> success -< Wildcard


-- approximateTermEnv :: HashSet StratVar -> Strat -> Interp () ()
-- approximateTermEnv senv s0 = case s0 of
--   S.Fail -> fail
--   Id -> id
--   GuardedChoice s1 s2 s3 ->
--     guardedChoice (approximateTermEnv senv s1) (approximateTermEnv senv s2) (approximateTermEnv senv s3)
--   Seq s1 s2 ->
--     sequence (approximateTermEnv senv s1) (approximateTermEnv senv s2)
--   One s -> approximateTermEnv senv s <+> fail
--   Some s -> approximateTermEnv senv s <+> fail
--   All s -> approximateTermEnv senv s <+> fail
--   Scope xs s -> scope xs (approximateTermEnv senv s)
--   Build _ -> id
--   Match p -> proc _ -> approximateMatch -< p
--   Let ss body -> let_ ss (approximateTermEnv senv body)
--   Call f ss ps ->
--     if H.member f senv
--     then id
--     else call f ss ps (approximateTermEnv (H.insert f senv))
--   where
--     approximateMatch = proc p -> case p of
--       S.Var "_" -> returnA -< ()
--       S.Var x -> do
--         env <- getTermEnv -< ()
--         putTermEnv -< M.insert x Wildcard env
--         success <+> fail -< ()
--       S.Cons _ ts -> do
--         _ <- mapA approximateMatch -< ts
--         success <+> fail -< ()
--       S.Explode c ts -> do
--         approximateMatch -< c
--         approximateMatch -< ts
--         success <+> fail -< ()
--       _ -> returnA -< ()


-- Instances -----------------------------------------------------------------------------------------

instance Category AbsInterp where
  id = Interp $ \a -> return (Success a)
  f . g = Interp $ \a -> do
    b <- runInterp g a
    case b of
        Success b' -> runInterp f b'
        Fail -> return Fail

instance Try AbsInterp where
  fail = Interp $ \_ -> return Fail
  try f g h = Interp $ \a -> do
    b <- runInterp f a
    case b of
      Success b' -> runInterp g b'
      Fail -> runInterp h a

instance Arrow AbsInterp where
  arr f = Interp $ \(a,e) -> return $ Success (f a, e)
  first f = Interp $ \((a,b),e) -> (fmap.fmap) (\(c,e') -> ((c,b),e')) (runInterp f (a,e))
  second f = Interp $ \((a,b),e) -> (fmap.fmap) (\(c,e') -> ((a,c),e')) (runInterp f (b,e))

instance ArrowChoice AbsInterp where
  left f = Interp $ \(a,e) -> case a of
    Left b -> (fmap.fmap) (first Left) (runInterp f (b,e))
    Right c -> return $ Success (Right c,e)
  right f = Interp $ \(a,e) -> case a of
    Left c -> return $ Success (Left c,e)
    Right b -> (fmap.fmap) (first Right) (runInterp f (b,e))
  f +++ g = Interp $ \(a,e) -> case a of
    Left b  -> (fmap.fmap) (first Left)  (runInterp f (b,e))
    Right b -> (fmap.fmap) (first Right) (runInterp g (b,e))

instance ArrowZero AbsInterp where
  zeroArrow = Interp (const mempty)

instance ArrowPlus AbsInterp where
  f <+> g = Interp $ \x -> runInterp f x <> runInterp g x

instance ArrowApply AbsInterp where
  app = Interp $ \((f,x),tenv) -> runInterp f (x,tenv)

instance HasTermEnv TermEnv AbsInterp where
  getTermEnv = Interp $ \((),e) -> return $ Success (e,e)
  putTermEnv = Interp $ \(e,_) -> return $ Success ((),e)

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
