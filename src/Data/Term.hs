{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}
module Data.Term where

import Prelude hiding (fail)
import Data.Constructor
import Data.Text(Text)

import Control.Arrow
import Control.Arrow.Try

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
      _ -> fail -< ()

  term :: p (TermF t) t

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

