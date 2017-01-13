{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
module Interpreter where

import Prelude hiding (fail,(.),id,sum,zipWith, curry, uncurry, flip)

import           Term
import           Syntax hiding (Fail)

import           Control.Arrow

import           Control.Category

import           Classes

import           Data.Map (Map)
import qualified Data.Map as M
-- import qualified Data.Map.Lazy.Merge as M

type StratEnv = Map StratVar Strat

test :: (Products p, Try p) => p a a -> p a a
test f = (id &&& id) >>> try (second f) (p1 >>> success) fail
{-# INLINE test #-}

neg :: Try p => p a a -> p a a
neg f = try f fail success
{-# INLINE neg #-}

sequence :: Category p => p a b -> p b c -> p a c
sequence f g = f >>> g
{-# INLINE sequence #-}

choice :: ArrowPlus p => p a b -> p a b -> p a b
choice f g = f <+> g
{-# INLINE choice #-}

leftChoice :: Try p => p a b -> p a b -> p a b
leftChoice f g = try f success (try g success fail)
{-# INLINE leftChoice #-}

recur :: Arrow p => StratEnv -> StratVar -> Strat -> (StratEnv -> p a b) -> p a b
recur env x s p = proc t -> p (M.insert x s env) -< t

var :: StratEnv -> StratVar -> (Strat -> p a b) -> p a b
var env x p = go
  where
    go = case M.lookup x env of
      Just s  -> p s
      Nothing -> error "Recursion variable was not in scope"
{-# INLINE var #-}

scope :: (TermEnv (Map TermVar t) p, Arrow p) => [TermVar] -> p a b -> p a b
scope = undefined
-- scope vars s = proc t -> do
--   env  <- getTermEnv -< ()
--   ()   <- putTermEnv -< foldr M.delete env vars
--   t'   <- s          -< t
--   env' <- getTermEnv -< ()
--   ()   <- putTermEnv -< merge env' env
--   returnA -< t'
--   where
--     merge = M.merge M.preserveMissing M.dropMissing $ M.zipWithMatched $ \k x y ->
--       if k `elem` vars then y else x
-- {-# INLINE scope #-}

path :: (CCC p, Try p, HasLists p, HasNumbers p) => Int -> p a a -> p (Constructor,[a]) (Constructor,[a])
path i f = second (lit i &&& id >>> nth f)
{-# INLINE path #-}

cong :: (CCC p, Try p, HasLists p) => Constructor -> [p a b] -> p (Constructor,[a]) (Constructor,[b])
cong c ss = proc (c',ts0) ->
  if c /= c' || length ts0 /= length ss
  then fail -< ()
  else second apply -< (c',zip ss ts0)
{-# INLINE cong #-}

plus :: (CCC p, ArrowPlus p) => p (p a b, p a b) (p a b)
plus = curry (((p1.p1 &&& p2) >>> eval) <+> ((p2.p1 &&& p2) >>> eval))

one :: (CCC p, Try p, ArrowPlus p, HasLists p, HasNumbers p) => p a a -> p (Constructor,[a]) (Constructor,[a])
one f = second (arr length &&& id >>> primRec' zeroArrow (first (curry (nth f)) >>> plus))
{-# INLINE one #-}

some :: (CCC p, Try p, HasLists p) => p t t -> p (Constructor,[t]) (Constructor,[t])
some f = second $ proc ts0 -> do
      (ts',oneSucceeded) <- go -< ts0
      if oneSucceeded
        then success -< ts'
        else fail -< ()
  where
    go = foldList (nil &&& false) (try (first f) (assoc >>> cons *** true) (assoc >>> first cons))
{-# INLINE some #-}

all :: (Arrow p,HasLists p) => p a b -> p (Constructor,[a]) (Constructor,[b])
all f = second (foldList nil (first f >>> cons))
{-# INLINE all #-}

nth :: (CCC p, Try p, HasLists p, HasNumbers p) => p a a -> p (Nat,[a]) [a]
nth f = primRec' (matchList >>> (fail ||| (first f >>> cons)))
                 (p2 >>> curry (second (matchList >>> (fail ||| id)) >>> shuffle >>> second eval >>> cons))
  where
    shuffle :: Products p => p (a,(b,c)) (b,(a,c))
    shuffle = p1.p2 &&& p1 &&& p2.p2
{-# NOINLINE nth #-}
