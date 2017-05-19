module SubtypeRelation(SubtypeRelation,empty,insert,subtype,lubs) where

import           Sort
import           Utils

import           Control.Monad

import           Data.Graph.Inductive (Node)
import qualified Data.Graph.Inductive as G
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import qualified Data.IntSet as S
import           Data.Maybe

data SubtypeRelation = SubtypeRelation Node (HashMap SortId Node) (Gr SortId ()) deriving (Show,Eq)

empty :: SubtypeRelation
empty = SubtypeRelation 0 M.empty G.empty

insert :: SortId -> SortId -> SubtypeRelation -> SubtypeRelation
insert s1 s2 rel0 =
  let (n1, rel1) = insertSort s1 rel0
      (n2, rel2) = insertSort s2 rel1
  in withGraph (G.tc . G.insEdge (n1,n2,())) rel2

withGraph :: (Gr SortId () -> Gr SortId ()) -> SubtypeRelation -> SubtypeRelation
withGraph f (SubtypeRelation m nodes rel) = SubtypeRelation m nodes (f rel)
{-# INLINE withGraph #-}

insertSort :: SortId -> SubtypeRelation -> (Node,SubtypeRelation)
insertSort s rel@(SubtypeRelation m nodes gr) =
  case M.lookup s nodes of
    Just n -> (n,rel)
    Nothing -> (m, SubtypeRelation (m+1) (M.insert s m nodes) (G.insNode (m,s) gr)) 

lookupSort :: SortId -> SubtypeRelation -> Maybe Node
lookupSort s (SubtypeRelation _ nodes _) = M.lookup s nodes

subtype :: SubtypeRelation -> Sort -> Sort -> Bool
subtype _ Bottom _ = True
subtype rel (List s1) (List s2) = subtype rel s1 s2
subtype rel (Option s1) (Option s2) = subtype rel s1 s2
subtype rel (Tuple s1) (Tuple s2) | eqLength s1 s2 = all (uncurry (subtype rel)) $ zip s1 s2
                                  | otherwise = False
subtype rel@(SubtypeRelation _ _ gr) (Sort s1) (Sort s2)
  | s1 == s2 = True
  | otherwise = fromMaybe False $ do
      n1 <- lookupSort s1 rel
      n2 <- lookupSort s2 rel
      return $ G.hasEdge gr (n1, n2)
subtype _ _ _ = False

lubs :: SubtypeRelation -> Sort -> Sort -> HashSet Sort
lubs rel s1 s2 = H.fromList $ lubs' rel s1 s2

lubs' :: SubtypeRelation -> Sort -> Sort -> [Sort]
lubs' rel@(SubtypeRelation _ _ gr) sort1 sort2 = case (sort1,sort2) of
  (Bottom,_) -> return sort2
  (_,Bottom) -> return sort1
  (List s1,  List s2)   -> List <$> lubs' rel s1 s2
  (Option s1,Option s2) -> Option <$> lubs' rel s1 s2
  (Tuple ts1,Tuple ts2) | eqLength ts1 ts2 -> Tuple <$> zipWithM (lubs' rel) ts1 ts2
  (Sort s1,Sort s2)
    | s1 == s2 -> return sort1
    | otherwise -> fromMaybe mempty $ do
       n1 <- lookupSort s1 rel
       n2 <- lookupSort s2 rel
       case () of
         _ | n1 == n2 -> return [sort1]
           | G.hasEdge gr (n1, n2) -> return [sort2]
           | G.hasEdge gr (n2, n1) -> return [sort1]
           | otherwise -> do
               let sup1 = S.fromList $ G.suc gr n1
                   sup2 = S.fromList $ G.suc gr n2
               mapM (fmap Sort . G.lab gr) $ S.toList $ sup1 `S.intersection` sup2
  _ -> mempty
