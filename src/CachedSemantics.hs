{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module CachedSemantics where

import           Prelude hiding ((.))

import           SharedSemantics hiding (all)
import           Syntax (Strat,StratEnv)
import           Syntax hiding (Fail,TermPattern(..))
import           WildcardSemantics

import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Debug
import           Control.Arrow.Deduplicate
import           Control.Arrow.Join
import           Control.Arrow.Try
import           Control.Category
import           Control.Monad.Identity
import           Control.Monad.Join (JoinT(..))
import qualified Control.Monad.Join as J
import           Control.Monad.Powerset
import           Control.Monad.Reader hiding (fail,sequence)
import           Control.Monad.Result
import           Control.Monad.State hiding (fail,sequence)

import           Data.Hashable
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Order
import           Data.Powerset
import           Data.Proxy
import           Data.Result
import           Data.Stack
import           Data.TermEnv
import           Debug.Trace

newtype Cache = Cache (HashMap (CallSignature,Term,TermEnv) (HashSet (Result (Term,TermEnv))))

instance PreOrd Cache where
  (Cache c1) ⊑ (Cache c2) =
    M.keys c1 == M.keys c2 &&
    all (\k -> (c1 M.! k) ⊑ (c2 M.! k)) (M.keys c1)

instance PartOrd Cache where

instance Lattice Cache where
  (Cache c1) ⊔ (Cache c2) = Cache (M.unionWith H.union c1 c2)

instance Monoid Cache where
  mempty = Cache M.empty
  mappend = (⊔) 

newtype Interp a b = Interp (Kleisli (ReaderT (StratEnv,Int) (StateT TermEnv (ResultT (PowT (JoinT Cache Identity))))) a b)
  deriving (Category,Arrow,ArrowChoice,ArrowApply,ArrowTry,ArrowJoin,ArrowDeduplicate)

runInterp :: Interp a b -> Int -> Cache -> StratEnv -> TermEnv -> a -> (Pow (Result (b,TermEnv)),Cache)
runInterp (Interp f) i cache senv tenv a = runIdentity (runJoinT (runPowT (runResultT (runStateT (runReaderT (runKleisli f a) (senv,i)) tenv))) cache)

eval :: Int -> Cache -> Strat -> StratEnv -> TermEnv -> Term -> Pow (Result (Term,TermEnv))
eval i cache s senv tenv t = fst $ runInterp (eval' Proxy s) i cache senv tenv t

liftK :: (a -> _ b) -> Interp a b
liftK f = Interp (Kleisli f)

instance HasStratEnv Interp where
  readStratEnv = liftK (const (fst <$> ask))
  localStratEnv senv (Interp (Kleisli f)) = liftK (local (first (const senv)) . f)


getCache :: Interp () Cache
getCache = liftK (const J.get)

putCache :: Interp Cache ()
putCache = liftK J.put

alternatives :: Interp (HashSet (Result (Term,TermEnv))) Term
alternatives = Interp $ Kleisli $ \xs -> ReaderT $ \_ -> StateT $ \_ -> ResultT $ PowT $ JoinT $ \c -> Identity (fromFoldable xs,c)

cacheResults :: CallSignature -> Interp Term Term -> Interp (Int,Term) Term
cacheResults sig f = Interp $ Kleisli $ \(i,a) -> ReaderT $ \(senv,_) -> StateT $ \tenv -> ResultT $ PowT $ JoinT $ \c -> Identity $
  let (r,Cache c') = runInterp f i c senv tenv a
  in (r,Cache $ M.insertWith H.union (sig,a,tenv) (toHashSet r) c')

getFuel :: Interp () Int
getFuel = liftK (const (snd <$> ask))

instance HasStack Term Interp where
  stackPush _ sig f = proc a -> do
    Cache cache <- getCache -< ()
    env <- getTermEnv -< ()
    case M.lookup (sig,a,env) cache of
      Just c -> alternatives -< traceShow (hash (sig,a,env)) c
      Nothing -> do
        i <- getFuel -< ()
        if i <= 0
        then returnA -< top
        else cacheResults sig f -< (i-1,a)

instance HasTermEnv TermEnv Interp where
  getTermEnv = liftK (const get)
  putTermEnv = liftK (modify . const)

instance ArrowDebug Interp where
  debug _ f = f
  --liftK $ \a -> ReaderT $ \r -> StateT $ \s -> ResultT $
  --   let b = trace (printf "CALL: %s(%s)" str (show (a,s))) $ runResultT (runStateT (runReaderT (f a) r) s)
  --       bFiltered = filterSuccess b
  --   in trace (printf "RETURN: %s(%s) -> %s" str (show (a,s)) (show bFiltered)) b
  --   where
  --     filterSuccess :: Pow (Result a) -> Pow a
  --     filterSuccess = foldMap $ \r -> case r of
  --       Success a -> return a
  --       Fail -> mempty


