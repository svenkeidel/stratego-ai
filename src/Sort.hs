{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sort where

import Data.Text(Text)
import Data.String(IsString(..))
import Data.Hashable(Hashable(..))

newtype SortId = SortId Text deriving (Show,Eq,Hashable,IsString)
data Sort = Bottom | List Sort | Option Sort | Tuple [Sort] | Sort SortId deriving (Show,Eq)

instance IsString Sort where
  fromString = Sort . fromString

instance Hashable Sort where
  hashWithSalt s x = case x of
    Bottom   -> s `hashWithSalt` (1::Int)
    Sort t   -> s `hashWithSalt` (2::Int) `hashWithSalt` t
    List t   -> s `hashWithSalt` (3::Int) `hashWithSalt` t
    Option t -> s `hashWithSalt` (4::Int) `hashWithSalt` t
    Tuple ts -> s `hashWithSalt` (5::Int) `hashWithSalt` ts
