{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
module Classes where

import Prelude hiding (fail,(.),id,sum,zipWith, curry, uncurry, flip, succ)

import           Control.Arrow
import           Control.Category

type CCC p = (Arrow p, ArrowChoice p, Exponentials p)

class Arrow p => Try p where
  success :: p a a
  fail :: p t a
  try :: p a b -> p b c -> p a c ->  p a c
              
class TermEnv env p | p -> env where
  getTermEnv :: p () env
  putTermEnv :: p env ()

class Arrow p => HasLists p where
  -- nil ||| cons is an isomorphism:
  -- nil ||| cons >>> matchList = id
  -- nil ||| cons <<< matchList = id

  -- nil ||| cons is an initial algebra:
  -- nil ||| cons >>> foldList f g = id +++ (id *** foldList f g) >>> (f ||| g)

  nil :: p t [a]
  cons :: p (a,[a]) [a]
  matchList :: p [a] (Either () (a,[a]))
  foldList :: p () b -> p (a,b) b -> p [a] b

  nil = arr (const [])
  cons = arr (uncurry (:))
  matchList = arr (\as -> case as of {[] -> Left (); (b:bs) -> Right (b,bs)})

type Nat = Int

class Arrow p => HasNumbers p where
  -- zero ||| succ is an isomorphism:
  -- zero ||| succ >>> matchList = id
  -- zero ||| succ <<< matchList = id

  -- zero ||| succ is an initial algebra:
  -- zero ||| succ >>> foldNat f g = (id +++ foldList f g) >>> (f ||| g) 

  lit :: Nat -> p a Nat
  zero :: p () Nat
  succ :: p Nat Nat
  matchNat :: p Nat (Either () Nat)
  foldNat :: p () b -> p b b -> p Nat b

  lit n = arr (const n)
  zero = lit 0
  succ = arr (+1)
  matchNat = arr (\as -> case as of {0 -> Left (); n -> Right (n-1)})

class Exponentials p where
  -- curry and uncurry is an bijection:
  -- curry (uncurry f) = f
  -- uncurry (curry f) = f

  -- (curry f *** id) >>> eval = f
  -- (g *** id) >>> eval = uncurry g

  curry :: p (a,b) c -> p a (p b c)
  uncurry :: p a (p b c) -> p (a,b) c
  eval :: p (p a b, a) b

primRec :: (Arrow p, HasNumbers p) => p () a -> p (Nat,a) a -> p Nat a
primRec f g = foldNat (zero &&& f) ((succ . p1) &&& g) >>> p2
{-# INLINE primRec #-}

primRec' :: (CCC p, HasNumbers p) => p a b -> p (Nat,p a b) (p a b) -> p (Nat,a) b
primRec' f g = eval . first (primRec (curry (f . p2)) g)
{-# INLINE primRec' #-}

in1 :: Arrow p => p a (Either a b)
in1 = arr Left

in2 :: Arrow p => p b (Either a b)
in2 = arr Right

p1 :: Arrow p => p (a,b) a
p1 = arr fst
{-# INLINE p1 #-}

p2 :: Arrow p => p (a,b) b
p2 = arr snd
{-# INLINE p2 #-}

true :: Arrow p => p a Bool
true = arr (const True)
{-# INLINE true #-}

false :: Arrow p => p a Bool
false = arr (const False)
{-# INLINE false #-}

assoc :: Arrow p => p (a,(b,c)) ((a,b),c)
assoc = (p1 &&& (p1 . p2)) &&& (p2 . p2)
{-# INLINE assoc #-}

apply :: (CCC p,HasLists p) => p [(p a b, a)] [b]
apply = foldList nil (first eval >>> cons)
{-# INLINE apply #-}

distribute :: (CCC p) => p (Either a b, c) (Either (a,c) (b,c))
distribute = first (curry in1 ||| curry in2) >>> eval

distribute' :: (CCC p) => p (a, Either b c) (Either (a,b) (a,c))
distribute' = flip >>> distribute >>> (flip +++ flip)

flip :: Arrow p => p (a,b) (b,a)
flip = p2 &&& p1

instance HasLists (->) where
  foldList f _ [] = f ()
  foldList f g (a:as) = g (a,foldList f g as)

instance HasNumbers (->) where
  foldNat f _ 0 = f ()
  foldNat f g n = g (foldNat f g (n-1))

instance Exponentials (->) where
  curry f a b = f (a,b)
  uncurry f (a,b) = f a b
  eval (f,a) = f a

instance Try (Kleisli Maybe) where
  success = Kleisli Just
  fail = Kleisli $ const Nothing
  try e s f = Kleisli $ \a ->
                case runKleisli e a of
                  Just b -> runKleisli s b
                  Nothing -> runKleisli f a

instance Try (Kleisli []) where
  success = Kleisli (: [])
  fail = Kleisli $ const []
  try e s f = Kleisli $ \a ->
                case runKleisli e a of
                  [] -> runKleisli f a
                  bs -> bs >>= runKleisli s

instance Monad m => HasLists (Kleisli m) where
  foldList f g = Kleisli $ \l -> case l of
    [] -> runKleisli f ()
    (a:as) -> do
        b <- runKleisli (foldList f g) as
        runKleisli g (a,b)

instance Monad m => HasNumbers (Kleisli m) where
  foldNat f g = Kleisli $ \n -> case n of
    0 -> runKleisli f ()
    _ -> do
      b <- runKleisli (foldNat f g) (n-1)
      runKleisli g b

instance Monad m => Exponentials (Kleisli m) where
  curry f = Kleisli $ \a -> return $ Kleisli $ \b -> runKleisli f (a,b)
  uncurry f = Kleisli $ \(a,b) -> do
                f' <- runKleisli f a
                runKleisli f' b
  eval = Kleisli $ uncurry runKleisli
