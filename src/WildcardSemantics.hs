{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module WildcardSemantics where

import           Prelude hiding (id,fail,concat,sequence,(.),uncurry)

import qualified ConcreteSemantics as C
import           Utils
import           Syntax hiding (Fail,TermPattern(..))

import           Control.DeepSeq
import           Control.Category
import           Control.Monad hiding (fail,sequence)
import           Control.Arrow
import           Control.Arrow.Try
import           Control.Arrow.Apply

import           Data.Complete
import           Data.Constructor
import           Data.Foldable (foldr')
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.Order
import           Data.Powerset hiding (size)
import qualified Data.Powerset as P
import           Data.Term (TermUtils(..))
import           Data.TermEnv
import           Data.Text (Text)

import           Test.QuickCheck hiding (Result(..))

data Term
    = Cons Constructor [Term]
    | StringLiteral Text
    | NumberLiteral Int
    | Wildcard
    deriving (Eq)

newtype TermEnv = TermEnv (HashMap TermVar Term) deriving (Show,Eq,Hashable)

emptyEnv :: TermEnv
emptyEnv = TermEnv M.empty

-- Instances -----------------------------------------------------------------------------------------
lookupTermVarDefault :: (ArrowApply c, HasTermEnv TermEnv c, Lattice (c () a)) => c Term a -> c () a -> c (TermVar,TermEnv) a
lookupTermVarDefault f g = proc (v,TermEnv env) ->
  case M.lookup v env of
    Just t -> f -< t
    Nothing ->
      (proc () -> do
        putTermEnv -< TermEnv (M.insert v Wildcard env)
        f -< Wildcard)
      ⊔ g
      -<< ()

insertTermDefault :: (ArrowApply c, HasTermEnv TermEnv c, Lattice (c () Term)) => c (TermVar,Term,TermEnv) TermEnv
insertTermDefault = arr $ \(v,t,TermEnv env) -> TermEnv (M.insert v t env)

deleteTermVarsDefault :: (ArrowApply c, HasTermEnv TermEnv c, Lattice (c () Term)) => c ([TermVar],TermEnv) TermEnv
deleteTermVarsDefault = arr $ \(vars,TermEnv env) -> TermEnv (foldr' M.delete env vars)

unionTermEnvsDefault :: (ArrowApply c, HasTermEnv TermEnv c, Lattice (c () Term)) => c ([TermVar],TermEnv,TermEnv) TermEnv
unionTermEnvsDefault = arr (\(vars,TermEnv e1,TermEnv e2) -> TermEnv (M.union e1 (foldr' M.delete e2 vars)))

matchTermAgainstConstructorDefault :: (ArrowChoice c, ArrowTry c, Lattice (c Term Term)) => c ([ps],[Term]) [Term] -> c (Constructor,[ps],Term) Term
matchTermAgainstConstructorDefault matchSubterms = proc (c,ts,t) -> case t of
  Cons c' ts' | c == c' && eqLength ts ts' -> do
    ts'' <- matchSubterms -< (ts,ts')
    returnA -< Cons c ts''
  Wildcard -> do
    ts'' <- matchSubterms -< (ts,[ Wildcard | _ <- [1..(length ts)] ])
    returnA ⊔ failA -< Cons c ts''
  _ -> failA -< ()

matchTermAgainstStringDefault :: (ArrowChoice c, ArrowTry c, Lattice (c Term Term)) => c (Text,Term) Term
matchTermAgainstStringDefault = proc (s,t) -> case t of
  StringLiteral s'
    | s == s' -> returnA -< t
    | otherwise -> failA -< ()
  Wildcard ->
    returnA ⊔ failA -< StringLiteral s
  _ -> failA -< ()

matchTermAgainstNumberDefault :: (ArrowChoice c, ArrowTry c, Lattice (c Term Term)) => c (Int,Term) Term
matchTermAgainstNumberDefault = proc (n,t) -> case t of
  NumberLiteral n'
    | n == n' -> returnA -< t
    | otherwise -> failA -< ()
  Wildcard ->
    success ⊔ failA -< NumberLiteral n
  _ -> failA -< ()

matchTermAgainstExplodeDefault :: (ArrowChoice c, ArrowTry c, Lattice (c Term Term)) => c Term Term -> c Term Term -> c Term Term  
matchTermAgainstExplodeDefault matchCons matchSubterms = proc t -> case t of
    Cons (Constructor c) ts -> do
      matchCons -< StringLiteral c
      matchSubterms -< convertToList ts
      returnA -< t
    StringLiteral _ -> do
      matchSubterms -< convertToList []
      returnA -< t
    NumberLiteral _ -> do
      matchSubterms -< convertToList []
      returnA -< t
    Wildcard ->
      (proc t -> do
         matchCons -< Wildcard
         matchSubterms -< Wildcard
         returnA -< t)
      ⊔
      (proc t -> do
         matchSubterms -< convertToList []
         returnA -< t)
      -< t

equalDefault :: (ArrowChoice c, ArrowTry c, Lattice (c Term Term)) => c (Term,Term) Term
equalDefault = proc (t1,t2) ->
  case (t1,t2) of
    (Cons c ts, Cons c' ts')
        | c == c' && eqLength ts ts' -> do
        ts'' <- zipWithA equalDefault -< (ts,ts')
        returnA -< Cons c ts''
        | otherwise -> failA -< ()
    (StringLiteral s, StringLiteral s')
        | s == s' -> success -< t1
        | otherwise -> failA -< ()
    (NumberLiteral n, NumberLiteral n')
        | n == n' -> success -< t1
        | otherwise -> failA -< ()
    (Wildcard, t) -> failA ⊔ success -< t
    (t, Wildcard) -> failA ⊔ success -< t
    (_,_) -> failA -< ()

convertFromListDefault :: (ArrowChoice c, ArrowTry c, Lattice (c Term Term)) => c (Term,Term) Term
convertFromListDefault = proc (c,ts) -> case (c,go ts) of
  (StringLiteral c', Just ts'') -> returnA -< Cons (Constructor c') ts''
  (Wildcard, Just _)            -> failA ⊔ success -< Wildcard
  (_,                Nothing)   -> failA ⊔ success -< Wildcard
  _                             -> failA -< ()
  where
    go t = case t of
      Cons "Cons" [x,tl] -> (x:) <$> go tl
      Cons "Nil" [] -> Just []
      Wildcard -> Nothing 
      _ -> Nothing

mapSubtermsDefault :: (ArrowChoice c, ArrowTry c, Lattice (c Term Term)) => c [Term] [Term] -> c Term Term
mapSubtermsDefault f = proc t ->
  case t of
    Cons c ts -> do
      ts' <- f -< ts
      returnA -< Cons c ts'
    StringLiteral _ -> returnA -< t
    NumberLiteral _ -> returnA -< t
    Wildcard -> failA ⊔ success -< Wildcard


consDefault :: Arrow c => c (Constructor,[Term]) Term
consDefault = arr (uncurry Cons)

numberLiteralDefault :: Arrow c => c Int Term
numberLiteralDefault = arr NumberLiteral

stringLiteralDefault :: Arrow c => c Text Term
stringLiteralDefault = arr StringLiteral

instance BoundedLattice Term where
  top = Wildcard

instance TermUtils Term where
  convertToList ts = case ts of
    (x:xs) -> Cons "Cons" [x,convertToList xs]
    []     -> Cons "Nil" []
    
  size (Cons _ ts) = sum (size <$> ts) + 1
  size (StringLiteral _) = 1
  size (NumberLiteral _) = 1
  size Wildcard = 1

  height (Cons _ []) = 1
  height (Cons _ ts) = maximum (height <$> ts) + 1
  height (StringLiteral _) = 1
  height (NumberLiteral _) = 1
  height Wildcard = 1

instance PreOrd Term where
  t1 ⊑ t2 = case (t1,t2) of
    (_,Wildcard) -> True
    (Cons c ts,Cons c' ts') -> c == c' && (ts ⊑ ts')
    (StringLiteral s, StringLiteral s') -> s == s'
    (NumberLiteral n, NumberLiteral n') -> n == n'
    (_, _) -> False

instance PartOrd Term

instance Lattice Term where
  t1 ⊔ t2 = case (t1,t2) of
    (Cons c ts, Cons c' ts')
      | c == c' -> case Complete ts ⊔ Complete ts' of
        Complete ts'' -> Cons c ts''
        _             -> Wildcard
      | otherwise -> Wildcard
    (StringLiteral s, StringLiteral s')
      | s == s' -> StringLiteral s
      | otherwise -> Wildcard
    (NumberLiteral n, NumberLiteral n')
      | n == n' -> NumberLiteral n
      | otherwise -> Wildcard
    (Wildcard, _) -> Wildcard
    (_, Wildcard) -> Wildcard
    (_, _) -> Wildcard

instance Lattice (Complete Term) where
  x ⊔ y = case (x,y) of
    (Complete t1, Complete t2) -> Complete (t1 ⊔ t2)
    (_,_) -> Top

instance Galois (Pow C.Term) Term where
  alpha = lub <<< P.map go
    where
      go = proc t -> case t of
        C.Cons c ts -> do
          ts' <- mapA go -< ts
          returnA -< Cons c ts'
        C.StringLiteral s -> returnA -< StringLiteral s
        C.NumberLiteral s -> returnA -< NumberLiteral s
  gamma = error "Infinite"

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

internal :: Arrow c => c (HashMap TermVar Term) (HashMap TermVar Term) -> c TermEnv TermEnv
internal f = arr TermEnv . f . arr (\(TermEnv e) -> e)

map :: ArrowChoice c => c Term Term -> c TermEnv TermEnv
map f = internal (arr M.fromList . mapA (second f) . arr M.toList)

newtype AbstractTermEnv t = AbstractTermEnv (HashMap TermVar t)
  deriving (Eq,Hashable,Show)

dom :: HashMap TermVar t -> [TermVar]
dom = M.keys

instance PreOrd TermEnv where
  TermEnv env1 ⊑ TermEnv env2 =
    Prelude.all (\v -> M.lookup v env1 ⊑ M.lookup v env2) (dom env2)

instance PartOrd TermEnv

instance Lattice TermEnv where
  TermEnv env1' ⊔ TermEnv env2' = go (dom env1') env1' env2' M.empty
    where
      go vars env1 env2 env3 = case vars of
        (v:vs) -> case (M.lookup v env1,M.lookup v env2) of
          (Just t1,Just t2) -> go vs env1 env2 (M.insert v (t1⊔t2) env3)
          _                 -> go vs env1 env2 env3
        [] -> TermEnv env3

-- instance (Eq t, Hashable t, Lattice t', Galois (Pow t) t') =>
--   Galois (Pow (ConcreteTermEnv t)) (AbstractTermEnv t') where
--   alpha cenvs = lub (fmap (\(ConcreteTermEnv e) -> AbstractTermEnv (fmap (alpha . P.singleton) e)) cenvs)
--   gamma = undefined

-- prim :: (ArrowTry p, ArrowAppend p, IsTerm t p, IsTermEnv (AbstractTermEnv t) t p)
--      => StratVar -> [TermVar] -> p a t
-- prim f ps = undefined
  -- proc _ -> case f of
  --   "SSL_strcat" -> do
  --     args <- lookupTermArgs -< ps
  --     case args of
  --       [T.StringLiteral t1, T.StringLiteral t2] -> stringLiteral -< t1 `append` t2
  --       [T.Wildcard, _] -> wildcard -< ()
  --       [_, T.Wildcard] -> wildcard -< ()
  --       _ -> fail -< ()
  --   "SSL_newname" -> do
  --     args <- lookupTermArgs -< ps
  --     case args of
  --       [T.StringLiteral _] -> wildcard -< ()
  --       [T.Wildcard] -> wildcard -< ()
  --       _ -> fail -< ()
  --   _ -> error ("unrecognized primitive function: " ++ show f) -< ()
  -- where
  --   lookupTermArgs = undefined
      -- proc args -> do
      -- tenv <- getTermEnv -< ()
      -- case mapM (`M.lookup` tenv) args of
      --   Just t -> mapA matchTerm -< t
      --   Nothing -> fail <+> success -< [T.Wildcard | _ <- args]
-- {-# SPECIALISE prim :: StratVar -> [TermVar] -> Interp StratEnv TermEnv PowersetResult Term Term #-}
