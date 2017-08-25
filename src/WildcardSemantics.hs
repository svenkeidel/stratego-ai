{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
module WildcardSemantics where

import           Prelude hiding (id,fail,concat,sequence,all,(.),uncurry)

import           InterpreterArrow
import qualified ConcreteSemantics as C
import           Utils

import           Control.DeepSeq
import           Control.Category
import           Control.Monad hiding (fail,sequence)
import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Join
import           Control.Arrow.Try
import           Control.Arrow.Apply

import           Data.Term (IsTerm(..),IsAbstractTerm(..),stringLiteral,TermUtils(..))
import           Data.TermEnv
import           Data.Constructor
import           Data.Powerset
import qualified Data.Powerset as P
import           Data.Hashable
import           Data.Text (Text)
import           Data.Order
import           Data.Complete

import           Test.QuickCheck hiding (Result(..))

data Term
    = Cons Constructor [Term]
    | StringLiteral Text
    | NumberLiteral Int
    | Wildcard
    deriving (Eq)

type TermEnv = AbstractTermEnv Term

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

-- Instances -----------------------------------------------------------------------------------------

instance (Monad m, ArrowTry (Interp r s m), ArrowJoin (Interp r s m)) => IsTerm Term (Interp r s m) where
  matchTermAgainstConstructor matchSubterms = proc (c,ts,t) -> case t of
    Cons c' ts'
      | c == c' && eqLength ts ts' -> do
        ts'' <- matchSubterms -< (ts,ts')
        returnA -< Cons c ts''
      | otherwise -> fail -< ()
    Wildcard ->
      (returnA <+> fail) -< Cons c [ Wildcard | _ <- [1..(length ts)] ]
    _ -> fail -< ()

  matchTermAgainstString = proc (s,t) -> case t of
    StringLiteral s'
      | s == s' -> returnA -< t
      | otherwise -> fail -< ()
    Wildcard ->
      stringLiteral <+> fail -< s
    _ -> fail -< ()

  matchTermAgainstNumber = proc (n,t) -> case t of
    NumberLiteral n'
      | n == n' -> returnA -< t
      | otherwise -> fail -< ()
    Wildcard ->
      numberLiteral <+> fail -< n
    _ -> fail -< ()
  
  matchTermAgainstExplode matchCons matchSubterms = proc t -> case t of
      Cons (Constructor c) ts -> do
        matchCons -< (StringLiteral c)
        matchSubterms -< convertToList ts
        returnA -< t
      StringLiteral _ -> do
        matchSubterms -< convertToList []
        returnA -< t
      NumberLiteral _ -> do
        matchSubterms -< convertToList []
        returnA -< t
      Wildcard ->
        (do matchCons -< Wildcard
            matchSubterms -< Wildcard)
        <+>
        (matchSubterms -< convertToList [])

  equal = proc (t1,t2) ->
    case (t1,t2) of
      (Cons c ts, Cons c' ts')
          | c == c' && eqLength ts ts' -> do
          ts'' <- zipWithA equal -< (ts,ts')
          cons -< (c,ts'')
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

  convertFromList = proc (c,ts) -> case (c,go ts) of
    (StringLiteral c', Just ts'') -> returnA -< Cons (Constructor c') ts''
    (Wildcard, Just _)            -> fail <+> success -< Wildcard
    (_,                Nothing)   -> fail <+> success -< Wildcard
    _                             -> fail -< ()
    where
      go t = case t of
        Cons "Cons" [x,tl] -> (x:) <$> go tl
        Cons "Nil" [] -> Just []
        Wildcard -> Nothing 
        _ -> Nothing

  lift f = proc t ->
    case t of
      Cons c ts -> do
        ts' <- f -< ts
        cons -< (c,ts')
      StringLiteral {} -> returnA -< t
      NumberLiteral {} -> returnA -< t
      Wildcard -> fail <+> success -< Wildcard

  cons = arr (uncurry Cons)
  numberLiteral = arr NumberLiteral
  stringLiteral = arr StringLiteral

instance (Monad m, ArrowTry (Interp r s m), ArrowJoin (Interp r s m)) => IsAbstractTerm Term (Interp r s m) where
  wildcard = arr (const Wildcard)

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
