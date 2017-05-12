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

class Arrow p => ArrowTry p where
  fail :: p t a
  try :: Monoid c => p a b -> p b c -> p a c ->  p a c

success :: ArrowTry p => p a a
success = id
{-# INLINE success #-}

class Arrow p => ArrowAppend p where
  -- zeroArrow :: (Monoid b) => p a b
  (<+>) :: Monoid b => p a b -> p a b -> p a b

getTermEnv :: ArrowState s p => p () s
getTermEnv = fetch
{-# INLINE getTermEnv #-}

putTermEnv :: ArrowState s p => p s ()
putTermEnv = store
{-# INLINE putTermEnv #-}

readSignature :: ArrowReader (Signature,StratEnv) p => p a Signature
readSignature = proc _ -> do
  (sig,_) <- readState -< ()
  returnA -< sig
{-# INLINE readSignature #-}

readStratEnv :: ArrowReader (Signature,StratEnv) p => p a StratEnv
readStratEnv = proc _ -> do
  (_,senv) <- readState -< ()
  returnA -< senv
{-# INLINE readStratEnv #-}

localStratEnv :: ArrowReader (Signature,StratEnv) p => p a b -> p (a,StratEnv) b 
localStratEnv f = proc (a,senv) -> do
  sig <- readSignature -< ()
  newReader f -< (a,(sig,senv))
{-# INLINE localStratEnv #-}

-- Language Constructs

guardedChoice :: (ArrowTry p, Monoid c) => p a b -> p b c -> p a c -> p a c
guardedChoice = try
{-# INLINE guardedChoice #-}

sequence :: Category p => p a b -> p b c -> p a c
sequence f g = f >>> g
{-# INLINE sequence #-}

one :: (ArrowTry p, ArrowAppend p, ArrowChoice p, Monoid a) => p a a -> p (Constructor,[a]) (Constructor,[a])
one f = second go
  where
    go = proc l -> case l of
      t:ts -> do
        (t',ts') <- first f <+> second go -< (t,ts)
        returnA -< (t':ts')
      [] -> fail -< ()
{-# INLINE one #-}

some :: (ArrowTry p, ArrowChoice p, Monoid t) => p t t -> p (Constructor,[t]) (Constructor,[t])
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

let_ :: ArrowReader (Signature,StratEnv) p => [(StratVar,Strategy)] -> Strat -> (Strat -> p a b) -> p a b
let_ ss body interp = proc a -> do
  let ss' = [ (v,Closure s' M.empty) | (v,s') <- ss ]
  senv <- readStratEnv -< ()
  localStratEnv (interp body) -< (a,M.union (M.fromList ss') senv) 
{-# INLINE let_ #-}

call :: (ArrowTry p, ArrowChoice p, ArrowApply p, ArrowState (HashMap TermVar t) p, ArrowReader (Signature,StratEnv) p)
     => StratVar
     -> [Strat]
     -> [TermVar]
     -> p (HashMap TermVar t, [(TermVar,TermVar)]) (HashMap TermVar t)
     -> (Strat -> p a b)
     -> p a b
call f actualStratArgs actualTermArgs bindTermArgs interp = proc a -> do
  senv <- readStratEnv -< ()
  case M.lookup f senv of
    Just (Closure (Strategy formalStratArgs formalTermArgs body) senv') -> do
      tenv <- getTermEnv -< ()
      putTermEnv <<< bindTermArgs -< (tenv,zip actualTermArgs formalTermArgs)
      let senv'' = bindStratArgs (zip formalStratArgs actualStratArgs)
                                 (if M.null senv' then senv else senv')
      b <- localStratEnv (interp body) -<< (a,senv'')
      tenv' <- getTermEnv -< ()
      putTermEnv -< tenv `M.union` foldr M.delete tenv' formalTermArgs
      returnA -< b
    Nothing -> error (printf "strategy %s not in scope" (show f)) -< ()

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

instance ArrowTry (Kleisli Maybe) where
  fail = Kleisli $ const Nothing
  try e s f = Kleisli $ \a ->
                case runKleisli e a of
                  Just b -> runKleisli s b
                  Nothing -> runKleisli f a

instance ArrowTry (Kleisli []) where
  fail = Kleisli $ const []
  try e s f = Kleisli $ \a ->
                case runKleisli e a of
                  [] -> runKleisli f a
                  bs -> bs >>= runKleisli s

instance ArrowAppend (Kleisli []) where
  -- zeroArrow = Kleisli $ const mempty
  Kleisli f <+> Kleisli g = Kleisli $ \a -> f a <> g a

instance (Monoid s, ArrowAppend p) => ArrowAppend (StateArrow s p) where
  -- zeroArrow = StateArrow zeroArrow
  StateArrow f <+> StateArrow g = StateArrow (f <+> g)
  {-# INLINE (<+>) #-}

instance (Monoid s, ArrowTry p) => ArrowTry (StateArrow s p) where
  fail = StateArrow (first fail)
  try (StateArrow f) (StateArrow g) (StateArrow h) = StateArrow (try f g h)
