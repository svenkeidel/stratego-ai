{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Syntax(Strat(..),StratVar) where

import Term

import Prelude hiding (maybe)

import Data.Text (Text,unpack)
import Data.List (intercalate)
import Data.String (IsString)

newtype StratVar = StratVar Text
  deriving (Eq,Ord,IsString)

instance Show StratVar where
  show (StratVar x) = unpack x

data Strat
    = Test Strat
    | Neg Strat
    | Fail
    | Id
    | Seq Strat Strat
    | Choice Strat Strat
    | LeftChoice Strat Strat
    | Rec StratVar Strat
    | RecVar StratVar
    | Path Int Strat
    | Cong Constructor [Strat]
    | One Strat
    | Some Strat
    | All Strat
    | Match (Term TermVar)
    | Build (Term TermVar)
    | Scope [TermVar] Strat

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
    Rec (StratVar v) s -> 
      showParen (d > app_prec)
        $ showString "rec "
        . showString (unpack v)
        . showString " "
        . showsPrec (app_prec+1) s
    RecVar (StratVar x) ->
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
      . showString (intercalate "," (map show vars))
      . showString ": "
      . shows s
      . showString " }"
    where
      app_prec = 10
      seq_prec = 9
      choice_prec = 8
