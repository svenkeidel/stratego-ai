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
import           Data.String (IsString(..))
import           Data.Hashable
import           Data.Semigroup(Semigroup(..))
import           Data.Text (Text)
import qualified Data.Text as T

import           Test.QuickCheck hiding (Result(..))

data Term
  = Cons Constructor [Term]
  | StringLiteral Text
  | NumberLiteral Int
  deriving (Eq)

type TermEnv = HashMap TermVar Term
newtype Interp a b = Interp { runInterp :: (Signature, StratEnv) -> (a,TermEnv) -> Result (b,TermEnv) }

eval :: Signature -> StratEnv -> Strat -> (Term,TermEnv) -> Result (Term,TermEnv)
eval sig senv s = runInterp (eval' s) (sig,senv)

eval' :: Strat -> Interp Term Term
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
  Prim f _ ps -> proc _ -> case f of
    "strcat" -> do
      tenv <- getTermEnv -< ()
      case mapM (`M.lookup` tenv) ps of
        Just [StringLiteral t1, StringLiteral t2] -> success -< StringLiteral (t1 `T.append` t2)
        _ -> fail -< ()
    "SSL_newname" -> do
      tenv <- getTermEnv -< ()
      case mapM (`M.lookup` tenv) ps of
        Just [StringLiteral _] -> undefined -< ()
        _ -> fail -< ()
    _ -> error ("unrecognized primitive function: " ++ show f) -< ()

match :: Interp (TermPattern, Term) Term
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

equal :: (ArrowChoice p, ArrowTry p) => p (Term,Term) Term
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
  S.As _ _ -> error "As-pattern in build is disallowed" -< ()
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

convertFromList :: (ArrowChoice p, ArrowTry p) => p Term [Term]
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

bindTermArgs :: (ArrowTry p, ArrowChoice p) =>
    p (HashMap TermVar t, [(TermVar,TermVar)]) (HashMap TermVar t)
bindTermArgs = proc (tenv,l) -> case l of
 (actual,formal) : rest -> case M.lookup actual tenv of
    Just t  -> bindTermArgs -< (M.insert formal t tenv, rest)
    Nothing -> fail -< ()
 [] -> returnA -< tenv

-- Instances -----------------------------------------------------------------------------------------

instance Category Interp where
  id = Interp $ \_ a -> Success a
  {-# INLINE id #-}
  Interp f . Interp g = Interp $ \r a ->
    case g r a of
      Success b -> f r b
      Fail -> Fail
  {-# INLINE (.) #-}

instance ArrowTry Interp where
  fail = Interp $ \_ _ -> Fail
  {-# INLINE fail #-}
  try (Interp f) (Interp g) (Interp h) = Interp $ \e a ->
    case f e a of
      Success b -> g e b 
      Fail -> h e a
  {-# INLINE try #-}

instance Arrow Interp where
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

instance ArrowChoice Interp where
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

instance ArrowAppend Interp where
  -- zeroArrow = fail
  Interp f <+> Interp g = Interp $ \x -> f x `mappend` g x
  {-# INLINE (<+>) #-}

instance ArrowState TermEnv Interp where
  fetch = Interp $ \_ (_,e) -> Success (e,e)
  {-# INLINE fetch #-}
  store = Interp $ \_ (e,_) -> Success ((),e)
  {-# INLINE store #-}

instance ArrowReader (Signature,StratEnv) Interp where
  readState = Interp $ \r (_,e) -> Success (r,e)
  {-# INLINE readState #-}
  newReader (Interp f) = Interp $ \_ ((a,r),e) -> f r (a,e)
  {-# INLINE newReader #-}

instance ArrowApply Interp where
  app = Interp $ \r ((f,b),e) -> runInterp f r (b,e)
  {-# INLINE app #-}

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

