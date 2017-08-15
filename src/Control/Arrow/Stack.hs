{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Arrow.Stack where

import Control.Arrow
import Data.Order
import Data.TermEnv
import Syntax


class IsTermEnv env t c => Alloc addr env t c where
  alloc :: c (StratVar, [Strat], [TermVar]) addr


class (ArrowChoice c, BoundedLattice x c, Eq addr, IsTermEnv env t c) => HasStack addr x env t c | c -> x where
  readAddr :: c addr (Maybe x)
  writeAddr :: c (addr, x) ()

  getStack :: c a [addr]
  localPush :: addr -> c x y -> c x y
  
  pushPop :: addr -> c x x -> c x x
  pushPop addr f = proc x -> do
    stack <- getStack -< ()
    -- true if addr is currently being executed, meaning a further push would start a cycle
    if addr `elem` stack
      -- return top instead of running into cycle
      then top -< ()
      else do
        y <- localPush addr f -< x
        cell <- readAddr -< addr
        case cell of
          -- the address is fresh: store result of f
          Nothing -> do
            writeAddr -< (addr, y)
            returnA -< y
          -- the address was used before: join with previous result, store and return joined result
          Just y2 -> do
            yjoined <- (⊔) -< (y, y2)
            writeAddr -< (addr, yjoined)
            returnA -< yjoined
