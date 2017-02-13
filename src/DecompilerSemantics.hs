{-# LANGUAGE Arrows #-}
module DecompilerSemantics where

import Prelude hiding ((.),id)

import Control.Category
import Control.Arrow

newtype F p a b = F (p a b, p (a,b) a)

runF :: Arrow p => F p a b -> p a (a,b)
runF (F (f,f')) = proc a -> do
  b <- f -< a
  a' <- f' -< (a,b)
  returnA -< (a',b)

class NarrowInput p where
  narrow :: p a ()

instance Arrow p => NarrowInput (F p) where
  narrow = F (unit, pi1)

instance Arrow p => Category (F p) where
  id = F (id, pi2)
  F (f,f') . F (g,g') =
    F (f.g,
       proc (a,c) -> do
         b  <- g -< a
         b' <- f' -< (b,c)
         g' -< (a,b')
      )

instance Arrow p => Arrow (F p) where
  arr f = F (arr f, pi1)

  first (F (f,f')) =
    F (first f,
       proc ((b,d),(c,_)) -> do
         b' <- f' -< (b,c)
         returnA -< (b',d)
      )

  second (F (f,f')) =
    F (second f,
       proc ((d,b),(_,c)) -> do
         b' <- f' -< (b,c)
         returnA -< (d,b')
      )

unit :: Arrow p => p a ()
unit = arr (const ())

pi1 :: Arrow p => p (a,b) a
pi1 = arr fst

pi2 :: Arrow p => p (a,b) b
pi2 = arr snd

{-
  first (arr f) = arr (first f)

  first (arr f)
    = first (F (arr f, pi1))
    = F (first (arr f),
         proc ((b,d),(c,_)) -> do
           b' <- pi1 -< (b,c)
           returnA -< (b',d)
        )
    = F (arr (first f), pi1)
    = arr (first f)

  second (arr f) = arr (second f)

  second (arr f)
    = second (F (arr f, pi1))
    = F (second (arr f),
         proc ((d,b),(_,c)) -> do
           b' <- pi1 -< (b,c)
           returnA -< (d,b')
    = F (arr (second f), pi1)
    = arr (second f)


  first (f . g) = first f . first g

  first (F (f,f') . F (g,g')) =
    = first $ F (f.g,
         proc (a,c) -> do
           b <- g -< a
           b' <- f' -< (b,c)
           g' -< (a,b')
        )
    = F (first (f.g),
         let h = proc (a,c) -> do
           b <- g -< a
           b' <- f' -< (b,c)
           g' -< (a,b')
         in proc ((a,d),(c,_)) -> do
           c' <- h -< (a,c)
           returnA -< (c',d)
        )
    = F (first f . first g,
         proc ((a,d),(c,_)) -> do
           b  <- g  -< a
           b' <- f' -< (b,c)
           c' <- g' -< (a,b')
           returnA -< (c',d)
        )

    = F (first f,
         proc ((b,d),(c,_)) -> do
           b' <- f' -< (b,c)
           returnA -< (b',d)
        ) .
      F (first g,
         proc ((b,d),(c,_)) -> do
           b' <- g' -< (b,c)
           returnA -< (b',d)
        )
    = first (F (f,f')) . first (g,g')
-}
