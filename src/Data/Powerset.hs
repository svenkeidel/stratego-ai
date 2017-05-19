module Data.Powerset where

import           Control.Arrow 

import           Data.Sequence 
import           Data.Hashable
import qualified Data.HashSet as H
import           Data.Foldable (foldl')

type Pow = Seq

union :: Seq a -> Seq a -> Seq a
union = (><)

class Arrow p => Deduplicate p where
  dedup :: (Hashable b,Eq b) => p a b -> p a b

dedup' :: (Hashable a,Eq a) => Seq a -> Seq a
dedup' = foldl' (|>) empty
       . foldl' (flip H.insert) H.empty
