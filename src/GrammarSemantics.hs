{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GrammarSemantics where

import Prelude hiding (fail,sequence)

import Term
import Syntax
import Interpreter hiding (Fail)

import Control.Monad hiding (fail,sequence)

import Data.Text (Text,unpack,append)
import Data.Map (Map)

-- A -> f[A,B]
-- A -> a
-- B -> g[A,B]
-- B -> b

newtype NonTerminal = NonTerminal Text
  deriving (Eq,Ord)

instance Show NonTerminal where
  show (NonTerminal x) = unpack x

data Grammar = Grammar NonTerminal (Map NonTerminal (Term NonTerminal))

instance Show Grammar where
  show (Grammar start p) =
    unlines $ flip map p $ \(Production v t) ->
      show $ if start == v
        then Production (markStartSym v) t
        else Production v t
    where
      markStartSym (NonTerminal x) = NonTerminal (x `append` "*")

interp :: (MonadPlus f,CanFail f) => Strat -> Grammar -> Interp env f Grammar
interp s0 g = case s0 of
  Test s -> test interp s g
  Neg s -> neg interp s g
  Fail -> fail
  Id -> success g
  Seq s1 s2 -> sequence interp s1 s2 g
  Choice s1 s2 -> choice interp s1 s2 g
  LeftChoice s1 s2 -> leftChoice interp s1 s2 g
  Rec {} -> undefined --limit (recur interp x s t) Hole
  RecVar {} -> undefined --limit (var interp x t) Hole
  Path {} -> undefined
  --   Cons c ts -> uncurry Cons <$> path interp i s c ts
  --   Hole -> fail `mplus` success Hole
  -- Cong c ss -> case t of
  --   Cons c' ts -> uncurry Cons <$> cong interp c ss c' ts
  --   Hole -> fail `mplus` interp (Cong c ss) (Cons c [Hole | _ <- ss])
  -- One s -> case t of
  --   Cons c ts -> uncurry Cons <$> one interp s c ts
  --   Hole -> fail `mplus` success Hole
  -- Some s -> case t of
  --   Cons c ts -> uncurry Cons <$> some interp s c ts
  --   Hole -> fail `mplus` success Hole
  -- All s -> case t of
  --   Cons c ts -> uncurry Cons <$> all interp s c ts
  --   Hole -> fail `mplus` success Hole
  -- Scope xs s -> scope interp xs s t
  -- Match f -> match f t
  -- Build f -> build f

