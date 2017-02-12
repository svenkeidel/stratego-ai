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
import           Data.Sequence (Seq)
import qualified Data.Sequence as S
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
newtype Interp a b = Interp {runInterp :: StratEnv -> (a,TermEnv) -> Seq (Result (b,TermEnv))}

eval :: Int -> Strat -> StratEnv -> (Term,TermEnv) -> Seq (Result (Term,TermEnv))
eval fuel s = runInterp (interp fuel s)

type Fuel = Int

interp :: Fuel -> Strat -> Interp Term Term
interp 0 _ = proc _ -> fail <+> success -< Wildcard
interp i s0 = dedup $ case s0 of
  S.Fail -> fail
  Id -> id
  GuardedChoice s1 s2 s3 -> guardedChoice (interp i s1) (interp i s2) (interp i s3)
  Seq s1 s2 -> sequence (interp i s1) (interp i s2)
  One s -> lift (one (interp i s))
  Some s -> lift (some (interp i s))
  All s -> lift (all (interp i s))
  Scope xs s -> scope xs (interp i s)
  Match f -> proc t -> match -< (f,t)
  Build f -> proc _ -> build -< f
  Let ss body -> let_ ss (interp i body)
  Call f ss ps -> call f ss ps (interp (i-1))
  where
    dedup :: Interp Term Term -> Interp Term Term
    dedup f = Interp $ \senv x -> dedup' $ runInterp f senv x

    dedup' :: Seq (Result (Term,TermEnv)) -> Seq (Result (Term,TermEnv))
    dedup' = foldl' (\s a -> s S.|> a) S.empty . foldl' (\m k -> H.insert k m) H.empty

match :: (ArrowChoice p, ArrowPlus p, Try p, HasTermEnv TermEnv p) => p (TermPattern,Term) Term
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

unify :: (ArrowPlus p, ArrowChoice p, Try p) => p (Term,Term) Term
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
  (Wildcard, t) -> fail <+> success -< t
  (t, Wildcard) -> fail <+> success -< t
  (_,_) -> fail -< ()

build :: (ArrowPlus p, ArrowChoice p, Try p, HasTermEnv TermEnv p) => p TermPattern Term
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
        case ts'' of
          Just tl -> success -< Cons (Constructor s) tl
          Nothing -> fail <+> success -< Wildcard
      Wildcard -> fail <+> success -< Wildcard
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

-- Instances -----------------------------------------------------------------------------------------

instance Category Interp where
  id = Interp (const (return . Success))
  f . g = Interp $ \senv x -> do
            y <- runInterp g senv x
            case y of
              Success t -> runInterp f senv t
              Fail -> return Fail

instance Arrow Interp where
  arr f = Interp (\_ (a,e) -> return $ Success (f a, e))
  first f = Interp $ \senv ((a,b),e) -> (fmap.fmap) (\(c,e') -> ((c,b),e')) (runInterp f senv (a,e))
  second f = Interp $ \senv ((a,b),e) -> (fmap.fmap) (\(c,e') -> ((a,c),e')) (runInterp f senv (b,e))

instance ArrowChoice Interp where
  left f = Interp $ \senv (a,e) -> case a of
    Left b -> (fmap.fmap) (first Left) (runInterp f senv (b,e))
    Right c -> return $ Success (Right c,e)
  right f = Interp $ \senv (a,e) -> case a of
    Left c -> return $ Success (Left c,e)
    Right b -> (fmap.fmap) (first Right) (runInterp f senv (b,e))
  f +++ g = Interp $ \senv (a,e) -> case a of
    Left b  -> (fmap.fmap) (first Left)  (runInterp f senv (b,e))
    Right b -> (fmap.fmap) (first Right) (runInterp g senv (b,e))

instance Try Interp where
  success = Interp (const (return . Success))
  fail = Interp (const (const (return Fail)))
  try t s f = Interp $ \senv (a,e) -> do
    x <- runInterp t senv (a,e)
    case x of
      Success y -> runInterp s senv y
      Fail -> runInterp f senv (a,e)

instance ArrowZero Interp where
  zeroArrow = Interp (const (const mempty))

instance ArrowPlus Interp where
  f <+> g = Interp $ \x -> runInterp f x <> runInterp g x

instance ArrowApply Interp where
  app = Interp $ \senv ((f,x),tenv) -> runInterp f senv (x,tenv)

instance HasTermEnv TermEnv Interp where
  getTermEnv = Interp $ \_ ((),e) -> return $ Success (e,e)
  putTermEnv = Interp $ \_ (e,_) -> return $ Success ((),e)

instance HasStratEnv Interp where
  readStratEnv = Interp $ \senv (_,tenv) -> return (Success (senv,tenv))
  localStratEnv f = Interp $ \_ ((x,senv),tenv) -> runInterp f senv (x,tenv)

instance Show Term where
  show (Cons c ts) = show c ++ if null ts then "" else show ts
  show (StringLiteral s) = show s
  show (NumberLiteral n) = show n
  show Wildcard = "_"

instance Ord Term where
  (Cons c ts) <= (Cons c' ts') = c <= c' && ts <= ts'
  (StringLiteral s) <= (StringLiteral s') = s <= s'
  (NumberLiteral n) <= (NumberLiteral n') = n <= n'
  _ <= Wildcard = True
  _ <= _ = False

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
