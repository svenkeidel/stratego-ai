{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Prelude hiding (id,(.),fail)
import Data.Text(Text)

main :: IO ()
main = putStrLn "Hello, Haskell!"

class RT l where

  successLeft :: l
  failLeft :: l
  success :: l
  (×) :: l -> l -> l
  infixr 3 ×
  (⊔) :: l -> l -> l
  infixr 3 ⊔
  (⊓) :: l -> l -> l
  infixr 3 ⊓
  id :: l
  (○) :: l -> l -> l
  infixr 9 ○
  fail :: l
  set :: Int -> l
  get :: Int -> l
  arityGreater :: Int -> l
  arityEqual :: Int -> l
  constructorCheck :: Text -> l
  

absSem :: RT l => Strat -> l
absSem strat = case strat of
  Test s ->
    successLeft ○ (id × absSem s) ⊔
    fail ○ absSem s
  Neg s ->
    failLeft ○ id × absSem s ⊔
    fail ○ absSem s
  Seq s1 s2 ->
    absSem s2 ○ absSem s1
  Choice s1 s2 ->
    success ○ absSem s1 ⊔
    success ○ absSem s2 ⊔
    (fail ○ absSem s1 ⊓ fail ○ absSem s2)
  LeftChoice s1 s2 ->
    success ○ absSem s1 ⊔
    (fail ○ absSem s1 ⊓ success ○ absSem s2) ⊔
    (fail ○ absSem s1 ⊓ fail ○ absSem s2)
  Path i s ->
    set i ○ id × (absSem s ○ get i) ⊔
    fail ○ (absSem s ○ get i) ⊔
    arityGreater i
  Cong f ss ->
    foldr (\(s,i) a -> map i s ○ a) id (zip ss [1..]) ⊔
    fst (foldr (\(s,i) (a,as) -> (fail ○ map i s ○ as ⊔ a, map i s ○ as)) (let i = id in (i,i)) (zip ss [1..])) ⊔
    constructorCheck f ⊔
    arityEqual (length ss)
  where
    map i s = set i ○ id × (absSem s ○ get i)
                                                                                                                                    
-- data Operations l =
--     Operations {
--     }

type Var = Text

data Strat
    = Test Strat
    | Neg Strat
    | Fail
    | Id
    | Seq Strat Strat
    | Choice Strat Strat
    | LeftChoice Strat Strat
    | Rec Var Strat
    | Var Var
    | Path Int Strat
    | Cong Text [Strat]
    | One Strat
    | Some Strat
    | All Strat
    | Match Term
    | Build Term
    | Where Strat
    | Scope [Var] Strat

data Term
    = Cons Text [Term]
    | TVar Var

-- class Lattice l where
--   top :: l
--   bot :: l
--   fail :: l
--   join :: l -> l -> l
--   meet :: l -> l -> l
--   fpathp :: Int -> l -> (l -> l) -> l
--   fallp :: l -> (l -> l) -> l
--   fmatchp :: Term -> l -> l

-- fstratp :: Lattice l => Strat -> l -> l
-- fstratp strat r = case strat of
--   Fail -> bot
--   Test s -> bstratp s r top
--   Neg s  -> bstratm s r fail
--   Seq s1 s2 -> fstratp s2 $ fstratp s1 r
--   Choice s1 s2 -> fstratp s1 r `join` fstratp s2 r
--   LeftChoice s1 s2 -> fstratp s1 r `join` (fstratm s1 r `meet` fstratp s2 r)
--   Rec v s1 -> fstratp (subst s1 v (Rec v s1)) r
--   Path i s -> fpathp i r (fstratp s)
--   All s -> fallp r (fstratp s)
--   Match t -> fmatchp t r

-- fstratm :: Lattice l => Strat -> l -> l
-- fstratm strat r = case strat of
--   Fail -> fail
--   Test s -> fstratm s r
--   Neg s -> fstratp s r
--   Seq s1 s2 -> 

-- bstratp :: Strat -> l -> l -> l
-- bstratp = undefined

-- bstratm :: Strat -> l -> l -> l
-- bstratm = undefined

-- subst :: Strat -> Var -> Strat -> Strat
-- subst strat v s' = case strat of
--   Test s -> Test (subst s v s')
--   Neg s -> Neg (subst s v s')
--   Fail -> Fail
--   Id -> Id
--   Seq s1 s2 -> Seq (subst s1 v s') (subst s2 v s')
--   Choice s1 s2 -> Choice (subst s1 v s') (subst s2 v s')
--   LeftChoice s1 s2 -> LeftChoice (subst s1 v s') (subst s2 v s')
--   Rec v' s | v' == v   -> Rec v' s
--            | otherwise -> Rec v' (subst s v s')
--   Var v' | v' == v   -> s'
--          | otherwise -> Var v'
--   Path i s -> Path i (subst s v s')
--   One s -> One (subst s v s')
--   Some s -> Some (subst s v s')
--   All s -> All (subst s v s')
--   Match t -> Match t
--   Build t -> Build t
--   Where s -> Where (subst s v s')
--   Scope vars s -> Scope vars (subst s v s')
