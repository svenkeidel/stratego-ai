{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
module Stack where

import Control.Arrow
import Data.Order
import Data.TermEnv
import Syntax

class HasAlloc addr c | c -> addr where
  alloc :: c (Strategy, [Strat], [TermVar]) addr

class (ArrowChoice c, BoundedLattice cell c, Eq addr)
      => HasStack ts addr cell c | c -> ts, c -> addr, c -> cell where
  getTimeStamp :: c a ts
  putTimeStamp :: c ts ()

  readAddr :: c addr (Maybe cell)
  writeAddr :: c (addr, cell) ()

  stack :: c a [addr]
  localPush :: addr -> c x y -> c x y
  
  localStackPush :: addr -> c cell cell -> c cell cell
  localStackPush addr f = proc x -> do
    addrs <- stack -< ()
    -- true if addr is currently being executed, meaning a further push would start a cycle
    if addr `elem` addrs
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
