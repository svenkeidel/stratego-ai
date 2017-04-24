{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows #-}
module Interpreter where

import           Prelude hiding (fail,(.),id,sum,zipWith, curry, uncurry, flip)

import           Syntax hiding (Fail)

import           Control.Arrow hiding (ArrowZero(..),ArrowPlus(..))
import           Control.Arrow.Operations
import           Control.Arrow.Transformer.State
import           Control.Category

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Semigroup

import           Text.Printf

class Arrow p => Try p where
  fail :: p t a
  try :: Monoid c => p a b -> p b c -> p a c ->  p a c

instance (Monoid s, Try p) => Try (StateArrow s p) where
  fail = StateArrow (first fail)
  try (StateArrow f) (StateArrow g) (StateArrow h) = StateArrow (try f g h)

success :: Try p => p a a
success = id

class Arrow p => ArrowAppend p where
  -- zeroArrow :: (Monoid b) => p a b
  (<+>) :: (Monoid b) => p a b -> p a b -> p a b

instance (Monoid s, ArrowAppend p) => ArrowAppend (StateArrow s p) where
  -- zeroArrow = StateArrow zeroArrow
  StateArrow f <+> StateArrow g = StateArrow (f <+> g)

getTermEnv :: ArrowState s p => p () s
getTermEnv = fetch

putTermEnv :: ArrowState s p => p s ()
putTermEnv = store
              
extendTermEnv :: (ArrowState (HashMap TermVar t) p) => p (TermVar,t) ()
extendTermEnv = proc (v,t) -> do
  env <- getTermEnv -< ()
  putTermEnv -< M.insert v t env
  
class HasStratEnv p where
  readStratEnv :: p () StratEnv
  localStratEnv :: p a b -> p (a, StratEnv) b

-- Language Constructs

guardedChoice :: (Try p, Monoid c) => p a b -> p b c -> p a c -> p a c
guardedChoice = try
{-# INLINE guardedChoice #-}

sequence :: Category p => p a b -> p b c -> p a c
sequence f g = f >>> g
{-# INLINE sequence #-}

one :: (Try p, ArrowAppend p, ArrowChoice p, Monoid a) => p a a -> p (Constructor,[a]) (Constructor,[a])
one f = second go
  where
    go = proc l -> case l of
      t:ts -> do
        (t',ts') <- first f <+> second go -< (t,ts)
        returnA -< (t':ts')
      [] -> fail -< ()
{-# INLINE one #-}

some :: (Try p, ArrowChoice p, Monoid t) => p t t -> p (Constructor,[t]) (Constructor,[t])
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

scope :: (ArrowState (HashMap TermVar t) p) => [TermVar] -> p a b -> p a b
scope vars s = proc t -> do
  env  <- getTermEnv -< ()
  ()   <- putTermEnv -< foldr M.delete env vars
  t'   <- s          -< t
  env' <- getTermEnv -< ()
  ()   <- putTermEnv -< env `M.union` foldr M.delete env' vars
  returnA -< t'
{-# INLINE scope #-}

let_ :: StratEnv -> [(StratVar,Strategy)] -> Strat -> (StratEnv -> Strat -> p a b) -> p a b
let_ senv ss body interp =
  let ss' = [ (v,Closure s' M.empty) | (v,s') <- ss ]
  in interp (M.union (M.fromList ss') senv) body

call :: (Try p, ArrowChoice p, ArrowApply p, ArrowState (HashMap TermVar t) p)
     => StratEnv -> StratVar -> [Strat] -> [TermVar] -> (StratEnv -> Strat -> p a b) -> p a b
call senv f actualStratArgs actualTermArgs interp = proc a ->
  case M.lookup f senv of
    Just (Closure (Strategy formalStratArgs formalTermArgs body) senv') -> do
      tenv <- getTermEnv -< ()
      putTermEnv <<< bindTermArgs -< (tenv,zip actualTermArgs formalTermArgs)
      let senv'' = bindStratArgs (zip formalStratArgs actualStratArgs)
                                 (if M.null senv' then senv else senv')
      b <- interp senv'' body -<< a
      tenv' <- getTermEnv -< ()
      putTermEnv -< tenv `M.union` foldr M.delete tenv' formalTermArgs
      returnA -< b
    Nothing -> error (printf "strategy %s not in scope" (show f)) -< ()

bindTermArgs :: (Try p, ArrowChoice p) =>
    p (HashMap TermVar t, [(TermVar,TermVar)]) (HashMap TermVar t)
bindTermArgs = proc (tenv,l) -> case l of
 (actual,formal) : rest -> case M.lookup actual tenv of
    Just t  -> bindTermArgs -< (M.insert formal t tenv, rest)
    Nothing -> fail -< ()
 [] -> returnA -< tenv

bindStratArgs :: [(StratVar,Strat)] -> StratEnv -> StratEnv
bindStratArgs [] senv = senv
bindStratArgs ((v,Call v' [] []) : ss) senv =
  case M.lookup v' senv of
    Just s -> M.insert v s (bindStratArgs ss senv)
    _ -> error $ "unknown strategy: " ++ show v'
bindStratArgs ((v,s) : ss) senv =
    M.insert v (Closure (Strategy [] [] s) senv) (bindStratArgs ss senv)

-- Auxiliary Functions
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
  fail = Kleisli $ const Nothing
  try e s f = Kleisli $ \a ->
                case runKleisli e a of
                  Just b -> runKleisli s b
                  Nothing -> runKleisli f a

instance Try (Kleisli []) where
  fail = Kleisli $ const []
  try e s f = Kleisli $ \a ->
                case runKleisli e a of
                  [] -> runKleisli f a
                  bs -> bs >>= runKleisli s

instance ArrowAppend (Kleisli []) where
  -- zeroArrow = Kleisli $ const mempty
  Kleisli f <+> Kleisli g = Kleisli $ \a -> f a <> g a
