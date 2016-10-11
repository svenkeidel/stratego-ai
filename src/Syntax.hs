{-# LANGUAGE OverloadedStrings #-}
module Syntax where

import Prelude hiding (maybe,concat)

import Data.Text (Text,unpack,concat)
import Data.List (intersperse)

import Control.Monad.State
import Control.Monad.Reader

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
    | Match TermV
    | Build TermV
--    | Where Strat
    | Scope [Var] Strat

instance Show Strat where
  showsPrec d s0 = case s0 of
    Test s ->
      showParen (d > app_prec)
      $ showString "test "
      . showsPrec (app_prec+1) s
    Neg s ->
      showParen (d > app_prec)
        $ showString "neg "
        . showsPrec (app_prec+1) s
    Fail ->
      showString "fail"
    Id ->
      showString "id"
    Seq s1 s2 ->
      showParen (d > seq_prec)
        $ showsPrec seq_prec s1
        . showString "; "
        . showsPrec seq_prec s2
    Choice s1 s2 ->
      showParen (d > choice_prec)
        $ showsPrec choice_prec s1
        . showString " + "
        . showsPrec choice_prec s2
    LeftChoice s1 s2 ->
      showParen (d > choice_prec)
        $ showsPrec (choice_prec+1) s1
        . showString " <+ "
        . showsPrec (choice_prec+1) s2
    Rec v s -> 
      showParen (d > app_prec)
        $ showString "rec "
        . showString (unpack v)
        . showString " "
        . showsPrec (app_prec+1) s
    Var x ->
      showString (unpack x)
    Path i s ->
      showParen (d > app_prec)
        $ showString "path "
        . showString (show i)
        . showsPrec (app_prec+1) s
    Cong c ss -> showString (show c) . showList ss
    One s ->
      showParen (d > app_prec)
        $ showString "one "
        . showsPrec (app_prec+1) s
    Some s ->
      showParen (d > app_prec)
        $ showString "some "
        . showsPrec (app_prec+1) s
    All s ->
      showParen (d > app_prec)
        $ showString "all "
        . showsPrec (app_prec+1) s
    Match t ->
      showString "?"
        . showsPrec (app_prec+1) t
    Build t ->
      showString "!"
        . showsPrec (app_prec+1) t
    Scope vars s ->
      showString "{ "
      . showString (unpack (concat (intersperse "," vars)))
      . showString ": "
      . shows s
      . showString " }"
    where
      app_prec = 10
      seq_prec = 9
      choice_prec = 8

type Constructor = Text

data TermV = ConsV Text [TermV]
           | VarV Var

instance Show TermV where
  show (ConsV c ts) = unpack c ++ if null ts then "" else show ts
  show (VarV x) = unpack x


class MonadMaybe m where
  maybe :: m a -> (Maybe a -> m b) -> m b

instance MonadMaybe Maybe where
  maybe m f = f m

instance MonadMaybe m => MonadMaybe (ReaderT r m) where
  maybe rd f = ReaderT
             $ \r -> maybe (runReaderT rd r)
             $ \a -> runReaderT (f a) r

instance MonadMaybe m => MonadMaybe (StateT s m) where
  maybe st f = StateT
             $ \s -> maybe (runStateT st s)
             $ \m -> case m of
                       Just (a,s') -> runStateT (f (Just a)) s'
                       Nothing -> runStateT (f Nothing) s
