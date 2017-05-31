{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
module Data.Term where

import Prelude hiding (fail)
import Data.Constructor
import Data.Text(Text)

import Control.Arrow hiding ((<+>))
import Control.Arrow.Try
import Control.Arrow.Append

import Utils

type (:+:) = Either
infixr :+:

type TermF t = (Constructor,[t]) :+: Text :+: Int :+: ()

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

  equal :: (ArrowChoice p, ArrowTry p, ArrowAppend p, Monoid t) => p (t,t) t
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

cons :: HasTerm t p => p (Constructor,[t]) t
cons = term <<^ uncurry Cons

stringLiteral :: HasTerm t p => p Text t
stringLiteral = term <<^ StringLiteral

numberLiteral :: HasTerm t p => p Int t
numberLiteral = term <<^ NumberLiteral

wildcard :: HasTerm t p => p () t
wildcard = term <<^ const Wildcard

pattern Cons :: Constructor -> [t] -> TermF t
pattern Cons c ts = Left (c,ts)

pattern StringLiteral :: Text -> TermF t
pattern StringLiteral s = Right (Left s)

pattern NumberLiteral :: Int -> TermF t
pattern NumberLiteral s = (Right (Right (Left s)))

pattern Wildcard :: TermF t
pattern Wildcard = Right (Right (Right ()))

