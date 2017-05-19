{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module ConcreteSemantics where

import           Prelude hiding (id,(.),fail,sequence,all,zipWith)

import           Interpreter
import           Syntax (TermPattern)
import qualified Syntax as S
import           Syntax hiding (Fail,TermPattern(..))
import           Utils

import           Control.Arrow
import           Control.Category
import           Control.Monad hiding (fail,sequence)

import           Data.Constructor
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.Maybe
import           Data.Result
import           Data.Semigroup (Semigroup(..))
import           Data.String (IsString(..))
import           Data.Term (HasTerm(..),TermF)
import qualified Data.Term as T
import           Data.Text (Text)
import qualified Data.Text as Text

import           Test.QuickCheck hiding (Result(..))

data Term
  = Cons Constructor [Term]
  | StringLiteral Text
  | NumberLiteral Int
  deriving (Eq)

type TermEnv = HashMap TermVar Term
newtype Interp r s a b = Interp { runInterp :: r -> (a,s) -> Result (b,s) }

eval :: StratEnv -> Strat -> (Term,TermEnv) -> Result (Term,TermEnv)
eval senv s = runInterp (eval' s) senv

eval' :: (ArrowChoice p, ArrowTry p, ArrowAppend p, ArrowApply p, HasTermEnv t p, HasStratEnv p, Monoid t) => Strat -> p t t
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
  Call f ss ps -> call f ss ps bindTermArgs eval'
  Prim f _ ps -> prim f ps

prim :: (ArrowTry p, HasTermEnv t p) => StratVar -> [TermVar] -> p t t
prim f ps = proc _ -> case f of
    "strcat" -> do
      tenv <- getTermEnv -< ()
      case mapM (`M.lookup` tenv) ps of
        Just [t1, t2] -> do
          m <- matchTerm *** matchTerm -< (t1,t2)
          case m of
            (T.StringLiteral s1,T.StringLiteral s2) ->
              T.stringLiteral -< s1 `Text.append` s2
            _ -> fail -< ()
        _ -> fail -< ()
    "SSL_newname" -> do
      tenv <- getTermEnv -< ()
      case mapM (`M.lookup` tenv) ps of
        Just [_] -> undefined -< ()
        _ -> fail -< ()
    _ -> error ("unrecognized primitive function: " ++ show f) -< ()

match :: (ArrowTry p, ArrowChoice p, ArrowAppend p, HasTermEnv t p, Monoid t) => p (TermPattern, t) t
match = proc (p,t) -> case p of
  S.As v p2 -> do
    t' <- match -< (S.Var v,t)
    match -< (p2,t')
  S.Var "_" -> success -< t
  S.Var x -> do
    env <- getTermEnv -< ()
    case M.lookup x env of
      Just t' -> equal -< (t,t')
      Nothing -> do
        putTermEnv -< M.insert x t env
        success -< t
  S.Cons c ts -> do
    m <- matchTerm -< t
    case m of
      T.Cons c' ts'
        | c == c' && eqLength ts ts' -> do
            ts'' <- zipWithA match -< (ts,ts')
            T.cons -< (c,ts'')
        | otherwise -> fail -< ()
      _ -> fail -< ()
  S.Explode c ts -> do
    m <- matchTerm -< t
    case m of
      T.Cons (Constructor c') ts' -> do
        s <- T.stringLiteral -< c'
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
        match -< (ts,l) 
        success -< t
      _ -> fail -< ()
  S.StringLiteral s -> do
    m <- matchTerm -< t
    case m of
      T.StringLiteral s'
        | s == s' -> success -< t
        | otherwise -> fail -< ()
      _ -> fail -< ()
  S.NumberLiteral n -> do
    m <- matchTerm -< t
    case m of
      T.NumberLiteral n'
        | n == n' -> success -< t
        | otherwise -> fail -< ()
      _ -> fail -< ()

build :: (ArrowChoice p, ArrowTry p, HasTermEnv t p) => p TermPattern t
build = proc p -> case p of
  S.As _ _ -> error "As-pattern in build is disallowed" -< ()
  S.Var x -> do
    env <- getTermEnv -< ()
    case M.lookup x env of
      Just t -> returnA -< t
      Nothing -> fail -< ()
  S.Cons c ts -> do
    ts' <- mapA build -< ts
    T.cons -< (c,ts')
  S.Explode c ts -> do
    m <- matchTerm <<< build -< c
    case m of
      T.StringLiteral s -> do
        ts' <- build -< ts
        ts'' <- arr fromJust <<< convertFromList -< ts'
        T.cons -< (Constructor s,ts'')
      _ -> fail -< ()
  S.NumberLiteral n -> T.numberLiteral -< n
  S.StringLiteral s -> T.stringLiteral -< s

bindTermArgs :: (ArrowTry p, ArrowChoice p)
             => p (HashMap TermVar t, [(TermVar,TermVar)]) (HashMap TermVar t)
bindTermArgs = proc (tenv,l) -> case l of
 (actual,formal) : rest -> case M.lookup actual tenv of
    Just t  -> bindTermArgs -< (M.insert formal t tenv, rest)
    Nothing -> fail -< ()
 [] -> returnA -< tenv

-- Instances -----------------------------------------------------------------------------------------

matchTermDefault :: Term -> TermF Term
matchTermDefault t = case t of
  Cons c ts -> T.Cons c ts
  StringLiteral s -> T.StringLiteral s
  NumberLiteral n -> T.NumberLiteral n
{-# INLINE matchTermDefault #-}


termDefault :: TermF Term -> Term
termDefault t = case t of
  T.Cons c ts -> Cons c ts
  T.NumberLiteral n -> NumberLiteral n
  T.StringLiteral s -> StringLiteral s
  _ -> error "cannot construct term"
{-# INLINE termDefault #-}

instance HasTerm Term (Interp r s) where
  matchTerm = arr matchTermDefault
  {-# INLINE matchTerm #-}
  term = arr termDefault
  {-# INLINE term #-}

instance HasTerm Term (->) where
  matchTerm = arr matchTermDefault
  {-# INLINE matchTerm #-}
  term = arr termDefault
  {-# INLINE term #-}
 
instance Category (Interp r s) where
  id = Interp $ \_ a -> Success a
  {-# INLINE id #-}
  Interp f . Interp g = Interp $ \r a ->
    case g r a of
      Success b -> f r b
      Fail -> Fail
  {-# INLINE (.) #-}

instance ArrowTry (Interp r s) where
  fail = Interp $ \_ _ -> Fail
  {-# INLINE fail #-}
  try (Interp f) (Interp g) (Interp h) = Interp $ \e a ->
    case f e a of
      Success b -> g e b 
      Fail -> h e a
  {-# INLINE try #-}

instance Arrow (Interp r s) where
  arr f = Interp (\_ (a,e) -> Success (f a, e))
  {-# INLINE arr #-}
  first (Interp f) = Interp $ \r ((a,b),e) -> fmap (\(c,e') -> ((c,b),e')) (f r (a,e))
  {-# INLINE first #-}
  second (Interp f) = Interp $ \r ((a,b),e) -> fmap (\(c,e') -> ((a,c),e')) (f r (b,e))
  {-# INLINE second #-}
  Interp f *** Interp g = Interp $ \r ((a,b),e) -> do
    (c,e')  <- f r (a,e)
    (d,e'') <- g r (b,e')
    Success ((c,d),e'')
  {-# INLINE (***) #-}
  Interp f &&& Interp g = Interp $ \r (a,e) -> do
    (b,e')  <- f r (a,e)
    (c,e'') <- g r (a,e')
    Success ((b,c),e'')
  {-# INLINE (&&&) #-}

instance ArrowChoice (Interp r s) where
  left (Interp f) = Interp $ \r (a,e) -> case a of
    Left b -> first Left <$> f r (b,e)
    Right c -> Success (Right c,e)
  {-# INLINE left #-}
  right (Interp f) = Interp $ \r (a,e) -> case a of
    Left c -> Success (Left c,e)
    Right b -> first Right <$> f r (b,e)
  {-# INLINE right #-}
  Interp f +++ Interp g = Interp $ \r (a,e) -> case a of
    Left b -> first Left  <$> f r (b,e)
    Right b -> first Right <$> g r (b,e)
  {-# INLINE (+++) #-}

instance ArrowAppend (Interp r s) where
  -- zeroArrow = fail
  Interp f <+> Interp g = Interp $ \x -> f x `mappend` g x
  {-# INLINE (<+>) #-}

instance ArrowApply (Interp r s) where
  app = Interp $ \r ((f,b),e) -> runInterp f r (b,e)
  {-# INLINE app #-}

instance HasTermEnv Term (Interp r TermEnv) where
  getTermEnv = Interp $ \_ (_,e) -> Success (e,e)
  {-# INLINE getTermEnv #-}
  putTermEnv = Interp $ \_ (e,_) -> Success ((),e)
  {-# INLINE putTermEnv #-}

instance HasStratEnv (Interp StratEnv s) where
  readStratEnv = Interp $ \r (_,e) -> Success (r,e)
  {-# INLINE readStratEnv #-}
  localStratEnv (Interp f) = Interp $ \_ ((a,r),e) -> f r (a,e)
  {-# INLINE localStratEnv #-}

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

