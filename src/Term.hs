{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Term (Term(..),Constructor,TermVar) where

import Data.Text (Text,unpack)
import Data.String (IsString)

newtype Constructor = Constructor Text
  deriving (Eq,IsString)

instance Show Constructor where
  show (Constructor c) = unpack c

data Term v = Cons Constructor [Term v]
           | Var v

instance Show v => Show (Term v) where
  show (Cons c ts) = show c ++ if null ts then "" else show ts
  show (Var x) = show x

newtype TermVar = TermVar Text
  deriving (Eq,Ord,IsString)

instance Show TermVar where
  show (TermVar x) = unpack x
