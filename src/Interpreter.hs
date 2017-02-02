{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}
module Interpreter where

import           Prelude hiding (fail,(.),id,sum,zipWith, curry, uncurry, flip)

import           Syntax hiding (Fail)

import           Control.Arrow

import           Control.Category

import           Data.Map (Map)
import qualified Data.Map as M
-- import qualified Data.Map.Lazy.Merge as M

class Arrow p => Try p where
  success :: p a a
  fail :: p t a
  try :: p a b -> p b c -> p a c ->  p a c
              
class HasTermEnv m p | p -> m where
  getTermEnv :: p () m
  putTermEnv :: p m ()

extendTermEnv :: (Arrow p, HasTermEnv (Map TermVar t) p) => p (TermVar,t) ()
extendTermEnv = proc (v,t) -> do
  env <- getTermEnv -< ()
  putTermEnv -< M.insert v t env
  
class HasStratEnv p where
  readStratEnv :: p () StratEnv
  localStratEnv :: p a b -> p (a, StratEnv) b

-- Language Constructs

guardedChoice :: Try p => p a b -> p b c -> p a c -> p a c
guardedChoice = try
{-# INLINE guardedChoice #-}

sequence :: Category p => p a b -> p b c -> p a c
sequence f g = f >>> g
{-# INLINE sequence #-}

path :: (Try p, ArrowChoice p, ArrowApply p) => Int -> p a a -> p (Constructor,[a]) (Constructor,[a])
path i f = second (nth' i f)
{-# INLINE path #-}

cong :: (Try p, ArrowChoice p, ArrowApply p) => Constructor -> [p a b] -> p (Constructor,[a]) (Constructor,[b])
cong c ss0 = proc (c',ts0) ->
  if c /= c' || length ts0 /= length ss0
  then fail -< ()
  else second apply -< (c', (ss0, ts0))
  where
    apply = proc (ss1,ts1) -> case (ss1,ts1) of
      (s:ss,t:ts) -> do
        t' <- app -< (s,t)
        ts' <- apply -< (ss,ts)
        returnA -< t':ts'
      ([],[]) -> returnA -< []
      _ -> fail -< () -- already checked that ts0 and ss have same length, i.e. cannot happen
{-# INLINE cong #-}

one :: (Try p, ArrowPlus p, ArrowChoice p) => p a a -> p (Constructor,[a]) (Constructor,[a])
one f = second go
  where
    go = proc l -> case l of
      t:ts -> do
        (t',ts') <- first f <+> second go -< (t,ts)
        returnA -< (t':ts')
      [] -> zeroArrow -< ()
{-# INLINE one #-}

some :: (Try p, ArrowChoice p) => p t t -> p (Constructor,[t]) (Constructor,[t])
some f = second go
  where
    go = proc l -> case l of
      (t:ts) -> do
        (t',ts') <- try (first f) (second go') (second go) -< (t,ts)
        returnA -< t':ts'
      [] -> fail -< () -- the strategy did not succeed for any of the subterms, i.e. some(s) failes
    go' = proc l -> case l of
      (t:ts) -> do
        (t',ts') <- try (first f) (second go') (second go') -< (t,ts)
        returnA -< t':ts'
      [] -> returnA -< []
{-# INLINE some #-}

all :: ArrowChoice p => p a b -> p (Constructor,[a]) (Constructor,[b])
all f = second (mapA f)
{-# INLINE all #-}

scope :: (Arrow p, HasTermEnv (Map TermVar t) p) => [TermVar] -> p a b -> p a b
scope vars s = proc t -> do
  env  <- getTermEnv -< ()
  ()   <- putTermEnv -< foldr M.delete env vars
  t'   <- s          -< t
  env' <- getTermEnv -< ()
  ()   <- putTermEnv -< merge env' env
  returnA -< t'
  where
    merge = undefined
      -- M.merge is available in containers 0.5.8.1 and later
      -- M.merge M.preserveMissing M.dropMissing $ M.zipWithMatched $ \k x y ->
      --   if k `elem` vars then y else x
{-# INLINE scope #-}

let_ :: (ArrowChoice p, HasStratEnv p) =>
        [(StratVar,Strategy)] -> p a b -> p a b
let_ ss s = proc t -> do
  senv <- readStratEnv -< ()
  localStratEnv s -< (t, M.union (M.fromList ss) senv)

call :: (ArrowChoice p, ArrowApply p, HasStratEnv p, HasTermEnv (Map TermVar t) p) =>
        StratVar -> [Strat] -> [TermVar] -> (Strat -> p a b) -> p a b
call f strategies actualTermArgs interp = proc a -> do
  senv <- readStratEnv -< ()
  case M.lookup f senv of
    Just (Strategy stratVars formalTermArgs body) -> do
      tenv <- getTermEnv -< ()
      terms <- lookupTermVars -< actualTermArgs
      () <- putTermEnv -< M.union (M.fromList (zip formalTermArgs terms)) tenv
      b <- localStratEnv (interp body) -<< (a,extendStratEnv stratVars senv)
 
      -- does not implement semantics, but saner to avoid term variables
      -- leaking out of strategy call.
      putTermEnv -< tenv

      returnA -< b
    Nothing -> error "strategy not in scope" -< ()
  where
    extendStratEnv stratVars =
      M.union $
       M.fromList $
        zip stratVars [ Strategy [] [] s| s <- strategies]

lookupTermVars :: HasTermEnv (Map TermVar t) p => p [TermVar] [t]
lookupTermVars = undefined

-- Auxiliary Functions

nth' :: (Try p, ArrowChoice p, ArrowApply p) => Int -> p a a -> p [a] [a]
nth' n f = proc l -> nth f -<< (n,l)
{-# NOINLINE nth' #-}

nth :: (Try p, ArrowChoice p) => p a a -> p (Int,[a]) [a]
nth f = proc (n,l) -> case (n,l) of
  (_,[]) -> fail -< ()
  (1, t:ts) -> do
    t' <- f -< t
    returnA -< t' : ts
  (i, t:ts) -> do
    ts' <- nth f -< (i-1,ts)
    returnA -< t : ts'

mapA :: (ArrowChoice p) => p a b -> p [a] [b]
mapA f = proc l -> case l of
  (t:ts) -> do
    t' <- f -< t
    ts' <- mapA f -< ts
    returnA -< (t':ts')
  [] -> returnA -< []

zipWith :: (ArrowChoice p) => p (a,b) c -> p ([a],[b]) [c]
zipWith f = proc x -> case x of
  (a:as,b:bs) -> do
    c <- f -< (a,b)
    cs <- zipWith f -< (as,bs)
    returnA -< c:cs
  _ -> returnA -< []

instance Try (Kleisli Maybe) where
  success = Kleisli Just
  fail = Kleisli $ const Nothing
  try e s f = Kleisli $ \a ->
                case runKleisli e a of
                  Just b -> runKleisli s b
                  Nothing -> runKleisli f a

instance Try (Kleisli []) where
  success = Kleisli (: [])
  fail = Kleisli $ const []
  try e s f = Kleisli $ \a ->
                case runKleisli e a of
                  [] -> runKleisli f a
                  bs -> bs >>= runKleisli s

