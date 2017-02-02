{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module WildcardSemantics where

import Prelude hiding (id,fail,concat,sequence,all,zipWith,(.))
import qualified Prelude as P

import Result
import Syntax hiding (Fail,TermPattern(..))
import Syntax (TermPattern)
import qualified Syntax as S
import Interpreter

import Control.Category
import Control.Arrow

import Data.Semigroup ((<>))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import Data.Text (Text)

data PartialTerm
    = Cons Constructor [PartialTerm]
    | StringLiteral Text
    | NumberLiteral Int
    | Wildcard
    deriving Eq

type TermEnv = Map TermVar PartialTerm
newtype Interp a b = Interp {runInterp :: StratEnv -> (a,TermEnv) -> Seq (Result (b,TermEnv))}

interp :: Strat -> Interp PartialTerm PartialTerm
interp s0 = case s0 of
  S.Fail -> fail
  Id -> id
  GuardedChoice s1 s2 s3 -> guardedChoice (interp s1) (interp s2) (interp s3)
  Seq s1 s2 -> sequence (interp s1) (interp s2)
  One s -> lift (one (interp s))
  Some s -> lift (some (interp s))
  All s -> lift (all (interp s))
  Scope xs s -> scope xs (interp s)
  Match f -> proc t -> match -< (f,t)
  Build f -> proc _ -> build -< f
  Let ss body -> let_ ss (interp body)
  Call f ss ps -> call f ss ps interp

match :: (ArrowChoice p, ArrowPlus p, Try p, HasTermEnv (Map TermVar PartialTerm) p) => p (TermPattern,PartialTerm) PartialTerm
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
      | c == c' || length ts == length ts' -> do
          ts'' <- zipWith match -< (ts,ts')
          success -< Cons c ts''
      | otherwise -> fail -< ()
    Wildcard -> do
      ts'' <- fail <+> zipWith match -< (ts,[Wildcard | _ <- ts])
      success -< Cons c ts''
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
    Wildcard -> fail <+> success -< StringLiteral s
    _ -> fail -< ()
  S.NumberLiteral n -> case t of
    NumberLiteral n'
      | n == n' -> success -< t
      | otherwise -> fail -< ()
    Wildcard -> fail <+> success -< NumberLiteral n
    _ -> fail -< ()

unify :: (ArrowPlus p, ArrowChoice p, Try p) => p (PartialTerm,PartialTerm) PartialTerm
unify = proc (t1,t2) -> case (t1,t2) of
  (Cons c ts,Cons c' ts')
    | c == c' || length ts == length ts' -> do
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

build :: (ArrowPlus p, ArrowChoice p, Try p,HasTermEnv (Map TermVar PartialTerm) p) => p TermPattern PartialTerm
build = proc p -> case p of
  S.Var x -> do
    env <- getTermEnv -< ()
    case M.lookup x env of
      Nothing -> fail -< ()
      Just t -> success -< t
  S.Cons c ts -> do
    ts' <- mapA build -< ts
    returnA -< (Cons c ts')
  S.Explode c ts -> do
    c' <- build -< c
    case c' of
      StringLiteral s -> do
        ts' <- build -< ts
        ts'' <- convertFromList -< ts'
        case ts'' of
          Just tl -> success -< Cons (Constructor s) tl
          Nothing -> fail <+> success -< Wildcard
      _ -> fail -< ()
  S.NumberLiteral n -> returnA -< NumberLiteral n
  S.StringLiteral s -> returnA -< StringLiteral s

convertToList :: [PartialTerm] -> PartialTerm
convertToList ts = case ts of
  (x:xs) -> Cons "Cons" [x,convertToList xs]
  [] -> Cons "Nil" []

convertFromList :: (ArrowChoice p, Try p) => p PartialTerm (Maybe [PartialTerm])
convertFromList = proc t -> case t of
  Cons "Cons" [x,tl] -> do
    xs <- convertFromList -< tl
    returnA -< (x:) <$> xs
  Cons "Nil" [] ->
    returnA -< Just []
  Wildcard -> returnA -< Nothing
  _ -> fail -< ()

lift :: (Try p,ArrowChoice p,ArrowPlus p)
     => p (Constructor,[PartialTerm]) (Constructor,[PartialTerm])
     -> p PartialTerm PartialTerm
lift p = proc t -> case t of
  Cons c ts -> do
    (c',ts') <- p -< (c,ts)
    returnA -< Cons c' ts'
  StringLiteral {} -> returnA -< t
  NumberLiteral {} -> returnA -< t
  Wildcard -> fail <+> success -< Wildcard

-- Instances -----------------------------------------------------------------------------------------

type Env = Map TermVar PartialTerm
data Environment = Singleton Env | Product Environment Environment

instance Show PartialTerm where
  show (Cons c ts) = show c ++ if null ts then "" else show ts
  show Wildcard = "_"

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