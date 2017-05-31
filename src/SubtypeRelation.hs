module SubtypeRelation(SubtypeRelation,empty,insert,subtype,lubs,lower) where

import           Sort
import           Utils

import           Control.Monad

import           Data.Graph.Inductive (Node)
import qualified Data.Graph.Inductive as G
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.IntSet (IntSet)
import qualified Data.IntSet as S
import           Data.Maybe

data SubtypeRelation = SubtypeRelation Node (HashMap Sort Node) (Gr Sort ()) deriving (Show,Eq)

empty :: SubtypeRelation
empty = SubtypeRelation 0 M.empty G.empty

insert :: Sort -> Sort -> SubtypeRelation -> SubtypeRelation
insert s1 s2 rel0 =
  let (n1, rel1) = insertSort s1 rel0
      (n2, rel2) = insertSort s2 rel1
  in withGraph (G.tc . G.insEdge (n1,n2,())) rel2

withGraph :: (Gr Sort () -> Gr Sort ()) -> SubtypeRelation -> SubtypeRelation
withGraph f (SubtypeRelation m nodes rel) = SubtypeRelation m nodes (f rel)
{-# INLINE withGraph #-}

insertSort :: Sort -> SubtypeRelation -> (Node,SubtypeRelation)
insertSort s rel@(SubtypeRelation m nodes gr) =
  case M.lookup s nodes of
    Just n -> (n,rel)
    Nothing -> (m, SubtypeRelation (m+1) (M.insert s m nodes) (G.insNode (m,s) gr)) 

lookupSort :: SubtypeRelation -> Sort -> Maybe Node
lookupSort (SubtypeRelation _ nodes _) s = M.lookup s nodes

subtype :: SubtypeRelation -> Sort -> Sort -> Bool
subtype rel@(SubtypeRelation _ _ gr) x y = case (x,y) of
  (Bottom,_) -> True
  (_,Top) -> True
  (List s1,List s2) -> subtype rel s1 s2
  (Option s1,Option s2) -> subtype rel s1 s2
  (Tuple s1,Tuple s2)
    | eqLength s1 s2 -> all (uncurry (subtype rel)) $ zip s1 s2
    | otherwise -> False
  (s1,Sort s2)
    | s1 == Sort s2 -> True
    | otherwise -> fromMaybe False $ do
        n1 <- lookupSort rel s1
        n2 <- lookupSort rel (Sort s2)
        return $ G.hasEdge gr (n1, n2)
  (_,_) -> False

lubs :: SubtypeRelation -> [Sort] -> [Sort]
lubs rel ss = case ss of
  [] -> []
  [x] -> [x]
  (x:y:r) -> filter (\s -> all (\s' -> subtype rel s' s) r) (lubs' rel x y) ++ lubs rel r
    --let res = filter (\s -> all (\s' -> subtype rel s' s) r) (lubs' rel x y) ++ lubs rel r
    -- The list res might contain duplicates and sorts which are subsumed by other sorts.
    -- The right thing to do here is to remove subsumed elements, however this operation
    -- is expensive.
    --in filter (\s -> not (any (\s' -> s /= s' && subtype rel s s') res)) (nub res)
    

lubs' :: SubtypeRelation -> Sort -> Sort -> [Sort]
lubs' rel@(SubtypeRelation _ _ gr) sort1 sort2 = case (sort1,sort2) of
  (Bottom,_) -> return sort2
  (_,Bottom) -> return sort1
  (Top,_) -> return Top
  (_,Top) -> return Top
  (List s1,  List s2)   -> List <$> lubs' rel s1 s2
  (Option s1,Option s2) -> Option <$> lubs' rel s1 s2
  (Tuple ts1,Tuple ts2) | eqLength ts1 ts2 -> Tuple <$> zipWithM (lubs' rel) ts1 ts2
  (s1,Sort s2)
    | s1 == Sort s2 -> return sort1
    | otherwise -> fromMaybe mempty $ do
       n1 <- lookupSort rel s1
       n2 <- lookupSort rel (Sort s2)
       case () of
         _ | n1 == n2 -> return [sort1]
           | G.hasEdge gr (n1, n2) -> return [sort2]
           | G.hasEdge gr (n2, n1) -> return [sort1]
           | otherwise -> do
               let sup1 = S.fromList $ G.suc gr n1
                   sup2 = S.fromList $ G.suc gr n2
               mapM (G.lab gr) $
                 S.toList $
                 removeNonLeastUpperBounds rel $
                 sup1 `S.intersection` sup2
  _ -> mempty

removeNonLeastUpperBounds :: SubtypeRelation -> IntSet -> IntSet
removeNonLeastUpperBounds (SubtypeRelation _ _ gr) ss =
    S.filter (\x -> not $ any (\y -> x /= y && G.hasEdge gr (y,x)) (S.toList ss)) ss
{-# INLINE removeNonLeastUpperBounds #-}

lower :: SubtypeRelation -> Sort -> [Sort]
lower rel@(SubtypeRelation _ _ gr) s0 = case s0 of
  Bottom -> [Bottom]
  Top -> error "lower set of top is unsupported"
  List s -> List <$> lower rel s
  Option s -> Option <$> lower rel s
  Tuple ss -> Tuple <$> permutations (lower rel <$> ss)
  s@(Sort _) -> fromMaybe mempty $ do
    n <- lookupSort rel s
    ss <- traverse (G.lab gr) (G.pre gr n)
    return (s:ss)

