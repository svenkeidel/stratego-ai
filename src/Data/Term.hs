{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
module Data.Term where

import Prelude hiding (fail)

import Data.Constructor
import Data.Text(Text)
import Data.Order

import Control.Arrow hiding ((<+>))
import Control.Arrow.Try

import Utils

type (:+:) = Either
infixr :+:

newtype TermF t = TermF ((Constructor,[t]) :+: Text :+: Int :+: ()) deriving (Eq)

class ArrowChoice p => HasTerm t p where
  matchTerm :: p t (TermF t)

  matchTermRefine :: p t (TermF t)
  matchTermRefine = matchTerm

  matchTermAgainstConstructor :: ArrowTry p => p (Constructor, t) (TermF t)
  matchTermAgainstConstructor = proc (c,t) -> do
    t' <- matchTerm -< t
    case t' of
      Cons c' _ | c == c' -> returnA -< t'
                | otherwise -> fail -< ()
      _ -> returnA -< t'

  term :: p (TermF t) t

instance Show s => Show (TermF s) where
  show (Cons c ts) = show c ++ if null ts then "" else show ts
  show (StringLiteral s) = show s
  show (NumberLiteral n) = show n
  show _ = "_"

instance (PreOrd t p, ArrowChoice p) => PreOrd (TermF t) p where
  (⊑) = proc (t1,t2) -> case (t1,t2) of
    (Cons c ts,Cons c' ts') -> do
      b <- (⊑) -< (ts,ts')
      returnA -< c == c' && b
    (StringLiteral s, StringLiteral s') -> returnA -< s == s'
    (NumberLiteral n, NumberLiteral n') -> returnA -< n == n'
    (_,Wildcard) -> returnA -< True
    (_, _) -> returnA -< False


instance (PartOrd t p, ArrowChoice p) => PartOrd (TermF t) p

cons :: HasTerm t p => p (Constructor,[t]) t
cons = term <<~ Cons

stringLiteral :: HasTerm t p => p Text t
stringLiteral = term <<^ StringLiteral

numberLiteral :: HasTerm t p => p Int t
numberLiteral = term <<^ NumberLiteral

wildcard :: HasTerm t p => p () t
wildcard = term <<^ const Wildcard

pattern Cons :: Constructor -> [t] -> TermF t
pattern Cons c ts = TermF (Left (c,ts))

pattern StringLiteral :: Text -> TermF t
pattern StringLiteral s = TermF (Right (Left s))

pattern NumberLiteral :: Int -> TermF t
pattern NumberLiteral s = TermF (Right (Right (Left s)))

pattern Wildcard :: TermF t
pattern Wildcard = TermF (Right (Right (Right ())))

size :: HasTerm t p => p t Int
size = proc t -> do
  t' <- matchTerm -< t
  case t' of
    Cons _ ts ->
      arr (succ . sum) <<< mapA size -< ts
    _ -> returnA -< 1
         
height :: HasTerm t p => p t Int
height = proc t -> do
  t' <- matchTerm -< t
  case t' of
    Cons _ ts
      | null ts -> returnA -< 1
      | otherwise -> arr (succ . maximum) <<< mapA height -< ts
    _ -> returnA -< 1

