{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Arrows #-}
module Cat where

import Prelude hiding (fail,(.),id,sum,zipWith)

import Term
import Syntax hiding (Fail)

import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Lazy.Merge as M

import Control.Arrow
import Control.Category

data Result t = Success t | Fail deriving Show

class Interpreter env p | p -> env where
  try :: p a b -> p a (Result b)
  success :: p a b
  fail :: p a b
  getTermEnv :: p a env
  putTermEnv :: p env b
  withStratEnv :: (StratEnv -> StratEnv) -> p a b -> p a b
  getStratEnv :: p a StratEnv

test :: (Interpreter env p,ArrowChoice p) => p a b -> p a b
test f = proc t -> do
  r <- try f -< t
  case r of
    Success _ -> success -< t
    Fail -> fail -< ()
{-# INLINE test #-}

neg :: (Interpreter env p, ArrowChoice p) => p a b -> p a b
neg f = proc t -> do
  env <- getTermEnv -< ()
  r <- try f -< t
  case r of
    Success _ -> fail -< ()
    Fail -> do
      () <- putTermEnv -< env 
      success -< t
{-# INLINE neg #-}

sequence :: Category p => p a b -> p b c -> p a c
sequence f g = f >>> g
{-# INLINE sequence #-}

choice :: (ArrowPlus p) => p a b -> p a b -> p a b
choice f g = f <+> g
{-# INLINE choice #-}

leftChoice :: (Interpreter env p,ArrowChoice p) => p a b -> p a b -> p a b
leftChoice f g = proc t -> do
  r1 <- try f -< t
  r2 <- try g -< t
  case (r1,r2) of
      (Success x,_) -> success -< x
      (Fail,Success x) -> success -< x
      (Fail,Fail) -> fail -< ()
{-# INLINE leftChoice #-}

recur :: (Interpreter env p,Arrow p) => StratVar -> Strat -> p a b -> p a b
recur x s p = proc t -> withStratEnv (M.insert x s) p -< t

var :: (Interpreter env p,ArrowChoice p) => StratVar -> p (Strat,a) b -> p a b 
var x f = proc t -> do
  m <- getStratEnv -< ()
  case M.lookup x m of
    Just s -> f -< (s,t)
    Nothing -> error "Recursion variable was not in scope" -< ()
{-# INLINE var #-}

scope :: (Interpreter (Map TermVar t) p, Arrow p) => [TermVar] -> p a b -> p a b
scope vars s = proc t -> do
  env  <- getTermEnv -< ()
  _    <- putTermEnv -< foldr M.delete env vars
  t'   <- s          -< t
  env' <- getTermEnv -< ()
  _    <- putTermEnv -< merge env' env
  returnA -< t'
  where
    merge = M.merge M.preserveMissing M.dropMissing $ M.zipWithMatched $ \k x y ->
      if k `elem` vars then y else x
{-# INLINE scope #-}

path :: (Interpreter env p, ArrowChoice p) => p (s,t) t -> p ((Int,s),Constructor,[t]) (Constructor,[t])
path f = proc ((i,s),c,ts) -> do
           ts' <- nth f -< (i,s,ts)
           returnA -< (c,ts')
{-# INLINE path #-}

nth :: (Interpreter env p, ArrowChoice p) => p (s,a) a -> p (Int,s,[a]) [a]
nth f = proc (i,s,l) -> case (i,l) of
  (1,x:xs) -> do
    x' <- f -< (s,x)
    returnA -< (x':xs)
  (_,x:xs) -> do
    xs' <- nth f -< (i-1,s,xs)
    returnA -< (x:xs')
  (_,_) ->
    fail -< ()

cong :: (Interpreter env p,ArrowChoice p) => p (Strat,t) t -> p ((Constructor,[Strat]),Constructor,[t]) (Constructor,[t])
cong f = proc ((c,ss0),c',ts0) ->
  if c /= c' || length ts0 /= length ss0
  then fail -< ()
  else do
    r <- go -< (ss0,ts0)
    returnA -< (c,r)  
  where
    go = proc x -> case x of
      (s:ss,t:ts) -> do
        t' <- f -< (s,t)
        ts' <- go -< (ss,ts)
        returnA -< (t':ts')
      _ -> returnA -< []
{-# INLINE cong #-}

one :: (Interpreter env p,ArrowChoice p,ArrowPlus p) => p (s,t) t -> p (s,Constructor,[t]) (Constructor,[t])
one f = proc (s,c,ts) -> do
  ts' <- sum (nth f) -< [(i,s,ts) | i <- [1..length ts]]
  returnA -< (c,ts')  
{-# INLINE one #-}

some :: (Interpreter env p,ArrowChoice p) => p (s,t) t -> p (s,Constructor,[t]) (Constructor,[t])
some f = proc (s,c,ts0) -> do
      (oneSucceeded,ts') <- go -< (s,ts0)
      if oneSucceeded
        then success -< (c,ts')
        else fail -< ()
  where 
    go = proc (s,l) -> case l of
      (t:ts) -> do
        r <- try f -< (s,t)
        case r of
          Success t' -> do
            (_,ts') <- go -< (s,ts)
            success -<(True,t':ts')
          Fail -> do
            (b,ts') <- go -< (s,ts)
            success -< (b,t:ts')
      [] -> success -< (False,[])
{-# INLINE some #-}

all :: (Interpreter env p,ArrowChoice p) => p (s,t) t -> p (s,Constructor,[t]) (Constructor,[t])
all f = proc (s,c,ts0) -> do
  ts' <- go -< (s,ts0)
  returnA -< (c,ts')
  where
    go = proc (s,l) -> case l of
      (t:ts) -> do
        t' <- f -< (s,t)
        ts' <- go -< (s,ts)
        success -< (t':ts')
      _ -> returnA -< []
{-# INLINE all #-}

sum :: (ArrowPlus p, ArrowChoice p) => p a b -> p [a] b
sum p = proc l -> case l of
  (x:xs) -> p . pi1 <+> sum p . pi2 -< (x,xs)
  [] -> zeroArrow -< ()
  where
    pi1 = arr fst
    pi2 = arr snd
{-# INLINE sum #-}

mapA :: ArrowChoice p => p a b -> p [a] [b]
mapA p = proc l -> case l of
  []     -> returnA -< []
  (x:xs) -> do
    x' <- p -< x
    xs' <- mapA p -< xs
    returnA -< x':xs'

zipWith :: ArrowChoice p => p (a,b) c -> p ([a],[b]) [c]
zipWith p = proc x -> case x of
  (a:as,b:bs) -> do
    c <- p -< (a,b)
    cs <- zipWith p -< (as,bs)
    returnA -< (c:cs)
  (_,_) ->
    returnA -< []

uncurry :: ArrowApply p => (a -> p b c) -> p (a,b) c
uncurry f = app <<< arr (first f)
