{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.TermEnv where

import           Syntax (TermVar)

import           Data.Hashable
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Order
import           Data.Term

newtype ConcreteTermEnv t = ConcreteTermEnv (HashMap TermVar t)
  deriving (Eq,Hashable,Show)
newtype AbstractTermEnv t = AbstractTermEnv (HashMap TermVar t)
  deriving (Eq,Hashable,Show)

instance PreOrd t => PreOrd (AbstractTermEnv t) where
  AbstractTermEnv e1 ⊑ AbstractTermEnv e2 = M.foldr (&&) True (M.intersectionWith (⊑) e1 e2)

instance PartOrd t => PartOrd (AbstractTermEnv t)

instance Lattice t => Lattice (AbstractTermEnv t) where
  AbstractTermEnv e1 ⊔ AbstractTermEnv e2 = AbstractTermEnv $ M.intersectionWith (⊔) e1 e2

class HasTerm t p => HasTermEnv env t p | p -> env, env -> t where
  getTermEnv :: p () env
  putTermEnv :: p env ()
  lookupTermVar :: p TermVar (Maybe t)
  insertTerm :: p (TermVar,t) ()
  deleteTermVars :: p [TermVar] ()
  unionTermEnvs :: p (env,env) env
