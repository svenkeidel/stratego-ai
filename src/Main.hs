{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Prelude hiding (Traversable(..),sum,all,or,and,fail,not,seq)
import Data.Text(Text,unpack)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Fix hiding (fix)

-- Interface

class Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

instance Alternative [] where
  empty = []
  l1 <|> l2 = l1 ++ l2

class Logic f where
  true :: a -> f a
  false :: f a
  not :: a -> f a -> f a
  or :: f a -> f a -> f a
  and :: f a -> f a -> f a

instance Logic Maybe where
  true = Just
  false = Nothing
  not _ (Just _) = Nothing
  not a Nothing = Just a
  or (Just a) _ = Just a
  or Nothing (Just a) = Just a
  or Nothing Nothing = Nothing
  and Nothing _ = Nothing
  and _ Nothing = Nothing
  and (Just a) (Just _) = Just a

class Traversable a where
  traverse :: Applicative f => (a -> f a) -> (a -> f a)

  -- destruct . construct = id
  -- construct . destruct = id
  deconstruct :: a -> (Text,[a])
  construct :: (Text,[a]) -> a

instance Traversable Term where
  traverse f (Cons c ts) = Cons c <$> traverseList f ts
    where
      traverseList g (x:xs) = (:) <$> g x <*> traverseList g xs
      traverseList _ [] = pure []
  deconstruct (Cons c ts) = (c,ts)
  construct (c,ts) = Cons c ts

children :: (Traversable a, Functor f) => ([a] -> f [a]) -> (a -> f a)
children f a = let (c,as) = deconstruct a
               in (\as' -> construct (c,as')) <$> f as

constructor :: Traversable a => a -> Text
constructor = fst . deconstruct


-- Language

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

data Term = Cons Text [Term] deriving Eq

instance Show Term where
  show (Cons c ts) = unpack c ++ if null ts then "" else show ts

t1 :: Term
t1 = f[g[c,d],e]
  where
    f = Cons "f"
    g = Cons "g"
    c = Cons "c" []
    d = Cons "d" []
    e = Cons "e" []

data TermV = ConsV Text [TermV]
           | VarV Var

fail :: Logic f => f a
fail = false

id :: Logic f => a -> f a
id = true

test :: Logic f => (a -> f a) -> (a -> f a)
test f t = true t `and` f t

neg :: Logic f => (a -> f a) -> (a -> f a)
neg f t = not t (f t)

lchoice :: Logic f => (a -> f a) -> (a -> f a) -> (a -> f a)
lchoice f g t = f t `or` g t

choice :: Alternative f => (a -> f a) -> (a -> f a) -> (a -> f a)
choice f g t = f t <|> g t

seq :: Monad f => (a -> f a) -> (a -> f a) -> (a -> f a)
seq f g = f >=> g

fix :: (MonadFix f) => ((a -> f a) -> (a -> f a)) -> (a -> f a)
fix f a = do
  mu <- mfix (return . f)
  mu a

path :: (Traversable a,Monad f, Logic f) => Int -> (a -> f a) -> (a -> f a)
path i s = children (nth i s)

congruence :: (Traversable a,Monad f,Logic f) => Text -> [a -> f a] -> (a -> f a)
congruence c ss t =
  let (c',ts) = deconstruct t
  in if c /= c' || length ts /= length ss
     then false
     else do
       ts' <- sequence $ ss <*> ts
       return $ construct (c',ts')

all :: (Traversable a,Monad f) => (a -> f a) -> (a -> f a)
all = traverse

one :: (Traversable a,Monad f,Alternative f,Logic f) => (a -> f a) -> (a -> f a)
one f = children (\chd -> alt [nth i f chd | i <- [1..length chd]])

some :: (Traversable a,Monad f,Logic f) => (a -> f a) -> (a -> f a)
some f = traverse (\t -> f t `or` pure t)

data Rule = TermV :-> TermV

rewrite :: (Traversable a, Eq a, Monad f,Logic f) => Rule -> (a -> f a)
rewrite (left :-> right) = match M.empty left >=> build right

match :: (Traversable a, Eq a, Monad f, Logic f) => Map Var a -> TermV -> a -> f (Map Var a)
match env (VarV x) t =
  case M.lookup x env of
    Just t' | t' == t -> true env
            | otherwise -> false
    Nothing -> true $ M.insert x t env
match env (ConsV c ts) t =
    let (c',ch) = deconstruct t
    in if c' /= c || length ch /= length ts
       then false
       else foldM (uncurry . match) env (zip ts ch)

build :: (Traversable a, Monad f,Logic f) => TermV -> Map Var a -> f a
build (VarV x) env =
  case M.lookup x env of
    Nothing -> false
    Just t -> true t
build (ConsV c ts) env = do
  as <- forM ts $ \t -> build t env
  return $ construct (c,as)


-- Library

topdown :: (Traversable a, MonadFix f) => (a -> f a) -> (a -> f a)
topdown s = fix $ \mu -> seq s (all mu)

bottomup :: (Traversable a, MonadFix f) => (a -> f a) -> (a -> f a)
bottomup s = fix $ \mu -> seq (all mu) s

-- Auxiliary functions
alt :: (Alternative f, Logic f) => [f a] -> f a
alt (x:xs) = x <|> alt xs
alt [] = false

nth :: (Monad f,Logic f) => Int -> (a -> f a) -> ([a] -> f [a])
nth 1 f (x:xs) = (:) <$> f x <*> pure xs
nth i f (x:xs) = (x:) <$> nth (i-1) f xs
nth _ _ [] = false


main :: IO ()
main = undefined
