{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module GrammarSemantics (Grammar, NonTerminal, (~>), grammar, interp) where

import Prelude hiding (fail,sequence,lookup)

import Term (Constructor)
import qualified Term as T
import Syntax
import Interpreter hiding (Fail,path)
import qualified Interpreter as I
import Single (Single)

import Control.Monad hiding (fail,sequence)
import Control.Arrow (first,second)
import Control.Monad.State hiding (fail,sequence)

import Data.Map (Map)
import qualified Data.Map as M

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as N

import Data.Semigroup
import Data.Maybe (fromMaybe)

import Text.Printf

newtype NonTerminal = NonTerminal Int
  deriving (Eq,Ord,Enum)

toInt :: NonTerminal -> Int
toInt (NonTerminal i) = i

instance Show NonTerminal where
  show (NonTerminal i) = show i

instance Semigroup NonTerminal where
  (NonTerminal i) <> (NonTerminal j) = succ (NonTerminal (max i j))

data Term = Cons Constructor [NonTerminal]
          | Hole

instance Show Term where
  show (Cons c ts) = show c ++ if null ts then "" else show ts
  show Hole = "_"

newtype Productions = Productions (IntMap (NonEmpty Term))

instance Semigroup Productions where
  (<>) = mappend

instance Monoid Productions where
  mempty = Productions IM.empty
  mappend (Productions p1) (Productions p2) = Productions (IM.union p1 p2)

data Grammar = Grammar NonTerminal Productions

instance MonadState (NonTerminal,env) f => Semigroup (f Grammar) where
  g1 <> g2 = do
    Grammar s1 p1 <- g1
    Grammar s2 p2 <- g2
    let g = Grammar s1 (p1 <> p2)
    newProduction g $ lookup s1 g <> lookup s2 g

(~>) :: NonTerminal -> Term -> (NonTerminal,Term)
(~>) n t = (n,t)

grammar :: NonTerminal -> [(NonTerminal,Term)] -> Grammar
grammar s = Grammar s . Productions . IM.fromListWith (<>) . fmap (first toInt . second return)

instance Show Grammar where
  show (Grammar start (Productions p)) =
    unlines $ flip fmap (IM.toList p) $ \(v,t) ->
      flip (printf "%d -> %s") (printAlternatives t) $
        if start == NonTerminal v then "*" ++ show v else show v
    where
     printAlternatives = unwords . N.toList . N.intersperse "|" . fmap show


-- adjust :: Applicative f => NonTerminal -> Grammar -> (Term NonTerminal -> f (Term NonTerminal)) -> f Grammar
-- adjust n (Grammar s prods) f = Grammar s <$> M.alterF (sequenceA . fmap (sequenceA . fmap f)) n prods

-- A* -> f[A,A]
-- path 1 (match f(x,y); build g(x,y))
--
-- A -> f[A,A]
-- B* -> f[C,A]
-- C -> g[A,A]

-- A* -> f[A,A] | b
-- path 1 (match f(x,y); build g(x,y))
--
-- Fail or
-- A -> f[A,A] | b
-- B* -> f[C,A]
-- C -> g[A,A]

-- A* -> f[A,b] | g[b,A]
-- match f(x,y); build h(x,y)
--
-- Fail or
-- A -> f[A,b] | g[b,A]
-- B* -> h[A,b]

-- A* -> f[A,b] | g[b,A]
-- match f(g[x,y],z); build h(x,z)
--
-- Fail or
-- A -> f[A,b] | g[b,A]
-- B* -> h[b,b]

lookup :: NonTerminal -> Grammar -> NonEmpty Term
lookup (NonTerminal n) (Grammar _ (Productions p)) = p IM.! n

startSymbol :: Grammar -> NonTerminal
startSymbol (Grammar s _) = s

withStartSymbol :: Grammar -> NonTerminal -> Grammar
withStartSymbol (Grammar _ ps) s = Grammar s ps

newProduction :: MonadState (NonTerminal,env) f => Grammar -> NonEmpty Term -> f Grammar
newProduction (Grammar _ (Productions ps)) newProd = do
  start <- fresh
  return $ Grammar start (addProduction start newProd)
  where
    fresh :: MonadState (NonTerminal,env) f => f NonTerminal
    fresh = do
      (n,env) <- get
      put (succ n,env)
      return n

    addProduction :: NonTerminal -> NonEmpty Term -> Productions
    addProduction (NonTerminal n) p = Productions (IM.insert n p ps)

each :: Semigroup (f Grammar) => Grammar -> NonTerminal -> (Term -> f Grammar) -> f Grammar
each gr nonTerminal f = sconcat $ fmap f (lookup nonTerminal gr)
  -- grs <- mapM f (lookup nonTerminal gr)
  -- newProduction (sconcat grs) $ do
  --   gr' <- grs
  --   lookup (startSymbol gr') gr'

path :: (CanFail f,MonadState (NonTerminal,env) f,Semigroup (f Grammar)) => (Strat -> Grammar -> f Grammar)
     -> Int -> Strat -> Grammar -> f Grammar
path f n s g = each g (startSymbol g) $ \t ->
  case t of
    Cons c ts ->
      let go 1 (x:xs) = do
            g' <- f s (g `withStartSymbol` x)
            return (startSymbol g':xs,g')
          go i (x:xs) = do
            (xs',g') <- go (i-1) xs
            return (x:xs',g')
          go _ [] = fail
      in do
        (ts',g') <- go n ts
        newProduction g' $ return $ Cons c ts' 
    Hole -> success g

type TermEnv = Map T.TermVar Grammar

grammarEq :: CanFail f => Grammar -> Grammar -> f Grammar
grammarEq = undefined

match :: (MonadState (NonTerminal,TermEnv) f,CanFail f,Semigroup (f Grammar)) => T.Term T.TermVar -> Grammar -> f Grammar
match f g = case f of
  T.Var x -> do
    (n,env) <- get
    case M.lookup x env of
      Just g' -> grammarEq g' g
      Nothing -> do
        put (n,M.insert x g env)
        success g
  T.Cons c' ts' -> each g (startSymbol g) $ \t -> case t of
      Cons c ts | c' /= c || length ts' /= length ts -> fail
                | otherwise -> do
                   gs <- zipWithM match ts' (withStartSymbol g <$> ts)
                   undefined gs
      Hole -> undefined


interp :: Strat -> Grammar -> Interp (NonTerminal,TermEnv) Single Grammar
interp s0 g = case s0 of
  Test s -> test interp s g
  Neg s -> neg interp s g
  Fail -> fail
  Id -> success g
  Seq s1 s2 -> sequence interp s1 s2 g
  Choice s1 s2 -> leftChoice interp s1 s2 g
  LeftChoice s1 s2 -> leftChoice interp s1 s2 g
  Rec {} -> undefined --limit (recur interp x s t) Hole
  RecVar {} -> undefined --limit (var interp x t) Hole
  Path i s -> path interp i s g
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
  Match f -> match f g
  -- Build f -> build f

