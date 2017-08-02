{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Data.TermEnv where

import           Prelude hiding ((.),id,map)
import           Syntax (TermVar)
import           Utils

import           Control.Category
import           Control.Arrow

import           Data.Hashable
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Order
import           Data.Complete hiding (map)
import           Data.Powerset (Pow)
import qualified Data.Powerset as P
import           Data.Term

newtype ConcreteTermEnv t = ConcreteTermEnv (HashMap TermVar t)
  deriving (Eq,Hashable,Show)

concreteTermEnv :: [(TermVar,t)] -> ConcreteTermEnv t
concreteTermEnv = ConcreteTermEnv . M.fromList

abstractTermEnv :: [(TermVar,t)] -> AbstractTermEnv t
abstractTermEnv = AbstractTermEnv . M.fromList

internal :: Arrow c => c (HashMap TermVar t) (HashMap TermVar t') -> c (AbstractTermEnv t) (AbstractTermEnv t')
internal f = arr AbstractTermEnv . f . arr (\(AbstractTermEnv e) -> e)

map :: ArrowChoice c => c t t' -> c (AbstractTermEnv t) (AbstractTermEnv t')
map f = internal (arr M.fromList . mapA (second f) . arr M.toList)

newtype AbstractTermEnv t = AbstractTermEnv (HashMap TermVar t)
  deriving (Eq,Hashable,Show)

dom :: AbstractTermEnv t -> [TermVar]
dom (AbstractTermEnv env) = M.keys env

instance (PreOrd t p, ArrowChoice p, ArrowApply p) => PreOrd (AbstractTermEnv t) p where
  (⊑) = proc (AbstractTermEnv env1,AbstractTermEnv env2) ->
    allA (proc v -> (⊑) -< (M.lookup v env1, M.lookup v env2)) -<< dom (AbstractTermEnv env2)

instance (PartOrd t p, ArrowChoice p, ArrowApply p) => PartOrd (AbstractTermEnv t) p

instance (Lattice t p, ArrowChoice p, ArrowApply p) => Lattice (AbstractTermEnv t) p where
  (⊔) = proc (env1'@(AbstractTermEnv env1), AbstractTermEnv env2) -> go -< (dom env1', env1,env2, M.empty)
    where
      go = proc (vars,env1,env2,env3) -> case vars of
        (v:vs) -> case (M.lookup v env1,M.lookup v env2) of
          (Just t1,Just t2) -> do
            t3 <- (⊔) -< (t1,t2)
            go -< (vs,env1,env2,M.insert v t3 env3)
          _ -> go -< (vs,env1,env2,env3)
        [] -> returnA -< AbstractTermEnv env3

instance (Eq t, Hashable t, Lattice t' c, Galois (Pow t) t' c, ArrowApply c, ArrowChoice c) =>
  Galois (Pow (ConcreteTermEnv t)) (AbstractTermEnv t') c where
  alpha = lub . P.map (map (alpha . P.unit) . arr (\(ConcreteTermEnv e) -> AbstractTermEnv e))
  gamma = undefined

class IsTerm t c => IsTermEnv env t c | c -> env, env -> t where
  getTermEnv :: c () env
  putTermEnv :: c env ()
  lookupTermVar :: Lattice (Complete a) c => c t a -> c () a -> c TermVar a
  insertTerm :: c (TermVar,t) ()
  deleteTermVars :: c [TermVar] ()
  unionTermEnvs :: c ([TermVar],env,env) env
