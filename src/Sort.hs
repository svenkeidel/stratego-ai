{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sort where

import Utils

import Data.Text(Text, unpack)
import Data.String(IsString(..))
import Data.Hashable(Hashable(..))
import Data.List(intercalate)


newtype SortId = SortId Text deriving (Show,Eq,Hashable,IsString)
data Sort = Bottom | Top | List Sort | Option Sort | Tuple [Sort] | Sort SortId deriving (Eq)

instance IsString Sort where
  fromString = Sort . fromString

instance Hashable Sort where
  hashWithSalt s x = case x of
    Bottom   -> s `hashWithSalt` (1::Int)
    Top      -> s `hashWithSalt` (2::Int)
    Sort t   -> s `hashWithSalt` (3::Int) `hashWithSalt` t
    List t   -> s `hashWithSalt` (4::Int) `hashWithSalt` t
    Option t -> s `hashWithSalt` (5::Int) `hashWithSalt` t
    Tuple ts -> s `hashWithSalt` (6::Int) `hashWithSalt` ts

instance Show Sort where
  show x = case x of
    Bottom -> "Bottom"
    Top -> "Top"
    List s -> "List (" ++ show s ++ ")"
    Option s -> "Option (" ++ show s ++ ")"
    Sort (SortId s) -> unpack s
    Tuple ss -> "(" ++ intercalate ", " (map show ss) ++ ")"

containsTop :: Sort -> Bool
containsTop s0 = case s0 of
  Top -> True
  List s -> containsTop s
  Option s -> containsTop s
  Tuple ss -> any containsTop ss
  Sort _ -> False
  Bottom -> False

getListType :: Sort -> Maybe Sort
getListType s = case s of
  List l -> Just l
  _ -> Nothing

getOptionType :: Sort -> Maybe Sort
getOptionType s = case s of
  Option l -> Just l
  _ -> Nothing

getTupleTypes :: [Sort] -> Sort -> Maybe [Sort]
getTupleTypes n s = case s of
  Tuple l | eqLength l n -> Just l
          | otherwise    -> Nothing
  _ -> Nothing

getSortId :: Sort -> Maybe SortId
getSortId s = case s of
  Sort i -> Just i
  _ -> Nothing
