{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Data.TermEnv where

import           Prelude hiding ((.),id,map)
import           Syntax (TermVar)

import           Control.Category

import           Data.Term

class HasTermEnv env c | c -> env where
  getTermEnv :: c () env
  putTermEnv :: c env ()

class IsTermEnv env t | env -> t where
  lookupTermVar :: Ar c => c t a -> c () a -> c (TermVar,env) a
  insertTerm :: Ar c => c (TermVar,t,env) env
  deleteTermVars :: Ar c => c ([TermVar],env) env
  unionTermEnvs :: Ar c => c ([TermVar],env,env) env

lookupTermVar' :: (Ar c, HasTermEnv env c, IsTermEnv env t) => c t a -> c () a -> c TermVar a
lookupTermVar' f g = proc v -> do
  env <- getTermEnv -< ()
  lookupTermVar f g -< (v,env)

insertTerm' :: (Ar c, HasTermEnv env c, IsTermEnv env t) => c (TermVar,t) ()
insertTerm' = proc (v,t) -> do
  env <- getTermEnv -< ()
  putTermEnv <<< insertTerm -< (v,t,env)

deleteTermVars' :: (Ar c, HasTermEnv env c, IsTermEnv env t) => c [TermVar] ()
deleteTermVars' = proc vs -> do
  env <- getTermEnv -< ()
  putTermEnv <<< deleteTermVars -< (vs,env)

