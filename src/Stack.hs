{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
module Stack where

import Control.Arrow
import Data.Order
import Syntax

import qualified Data.HashMap.Lazy as M
import           Data.Hashable


type StratCall = (Strategy, [Strat], [TermVar])

data StratCallCatchAddr
  = StratCallAddr StratCall Int
  | CatchAddr
  deriving (Eq, Show)

instance Hashable StratCallCatchAddr where
  hashWithSalt s (StratCallAddr call n) = s `hashWithSalt` (0::Int) `hashWithSalt` call `hashWithSalt` n
  hashWithSalt s CatchAddr = s `hashWithSalt` (1::Int)



class HasAlloc addr c | c -> addr where
  alloc :: c StratCall addr


data Stack ts addr cell = Stack
  { timestamp :: ts
  , store :: M.HashMap addr cell
  , stacked :: [addr] }
  deriving (Eq, Show)

emptyStack :: ts -> Stack ts addr cell
emptyStack ts = Stack ts M.empty []

instance (Hashable ts, Hashable addr, Hashable cell) => Hashable (Stack ts addr cell) where
  hashWithSalt s (Stack ts sto sta) = s `hashWithSalt` ts `hashWithSalt` sto `hashWithSalt` sta

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
