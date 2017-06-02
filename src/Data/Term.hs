{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
module Data.Term where

import Prelude hiding (fail)

import Data.Constructor
import Data.Text(Text)
import Data.Order

import Control.Arrow hiding ((<+>))
import Control.Arrow.Try
import Control.Arrow.Append

import Utils

type (:+:) = Either
infixr :+:

newtype TermF t = TermF ((Constructor,[t]) :+: Text :+: Int :+: ()) deriving (Eq)

class (ArrowChoice p) => HasTerm t p where
  matchTerm :: p t (TermF t)

  matchTermAgainstConstructor :: ArrowTry p => p (Constructor, t) (TermF t)
  matchTermAgainstConstructor = proc (c,t) -> do
    t' <- matchTerm -< t
    case t' of
      Cons c' _ | c == c' -> returnA -< t'
                | otherwise -> fail -< ()
      _ -> returnA -< t'

  term :: p (TermF t) t

  equal :: (ArrowChoice p, ArrowTry p, ArrowAppend p, Lattice t) => p (t,t) t
  equal = proc (t1,t2) -> do
    m <- matchTerm *** matchTerm -< (t1,t2)
    case m of
      (Cons c ts,Cons c' ts')
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
      (Wildcard, t) -> fail <+> term -< t
      (t, Wildcard) -> fail <+> term -< t
      (_,_) -> fail -< ()

instance Show s => Show (TermF s) where
  show (Cons c ts) = show c ++ if null ts then "" else show ts
  show (StringLiteral s) = show s
  show (NumberLiteral n) = show n
  show _ = "_"

cons :: HasTerm t p => p (Constructor,[t]) t
cons = term <<^ uncurry Cons

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

