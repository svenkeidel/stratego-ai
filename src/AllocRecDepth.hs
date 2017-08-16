{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module AllocRecDepth where

import Prelude hiding (last)

import Control.Arrow

import Stack

maxRecDepth :: Int
maxRecDepth = 3

type TimeStamp = Int
type Addr = StratCallCatchAddr

instance (ArrowChoice c, HasStack TimeStamp Addr cell c)
         => HasAlloc StratCallCatchAddr c where
--  alloc :: c StratCall addr
  alloc = proc call -> do
    addrs <- stack -< ()
    let count = length $ filter (\(StratCallAddr c _) -> c==call) addrs
    if count < maxRecDepth
      then do
        last <- getTimeStamp -< ()
        let next = last + 1
        putTimeStamp -< next
        returnA -< StratCallAddr call next
      else returnA -< CatchAddr
