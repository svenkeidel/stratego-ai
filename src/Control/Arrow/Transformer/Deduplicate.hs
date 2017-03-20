module Control.Arrow.Transformer.Deduplicate where

import Control.Arrow 
import Control.Arrow.Transformer.State

import Data.Hashable
import qualified Data.HashSet as H
import           Data.Sequence 
import           Data.Foldable (foldl')

class Arrow p => Deduplicate p where
  dedup :: (Hashable b,Eq b) => p a b -> p a b

instance (Hashable s, Eq s, Deduplicate p) => Deduplicate (StateArrow s p) where
  dedup (StateArrow p) = StateArrow (dedup p)

dedup' :: (Hashable a,Eq a) => Seq a -> Seq a
dedup' = foldl' (|>) empty
       . foldl' (flip H.insert) H.empty
