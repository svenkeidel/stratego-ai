{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GrammarSemantics where

import Syntax
import Data.Text (Text,unpack,append)
import Data.String (IsString)

-- A -> f[A,B]
-- A -> a
-- B -> g[A,B]
-- B -> b

newtype GrammarVar = GrammarVar Text
  deriving (Eq,Ord,IsString)

instance Show GrammarVar where
  show (GrammarVar x) = unpack x

data Production = Production GrammarVar (Term GrammarVar)

(~>) :: GrammarVar -> Term GrammarVar -> Production
(~>) = Production

instance Show Production where
  show (Production v t) = show v ++ " -> " ++ show t

data Grammar = Grammar GrammarVar [Production]

instance Show Grammar where
  show (Grammar start p) =
    unlines $ flip map p $ \(Production v t) ->
      show $ if start == v
        then Production (markStartSym v) t
        else Production v t
    where
      markStartSym (GrammarVar x) = GrammarVar (x `append` "*")
