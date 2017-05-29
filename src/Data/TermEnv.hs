{-# LANGUAGE FunctionalDependencies #-}
module Data.TermEnv where

import Syntax (TermVar)
import Data.HashMap.Lazy (HashMap)

class HasTermEnv t p | p -> t where
  getTermEnv :: p () (HashMap TermVar t)
  putTermEnv :: p (HashMap TermVar t) ()
