{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TypedSemantics where

import           Prelude hiding (id,(.),fail,all)

import           InterpreterArrow
import           ConcreteSemantics hiding (Term(..),TermEnv,eval)
import           Sort
import           Signature hiding (lookupType,lub)
import qualified Signature as Sig
import           Syntax(Module,Strat,StratEnv,TermVar,stratEnv,signature)
import           Utils

import           Data.Constructor
import           Data.HashMap.Lazy (HashMap)
import           Data.Order hiding (lub)
import qualified Data.Term as T
import           Data.Term(HasTerm(..))
import           Data.TermEnv
import           Data.Text(Text,pack)
import           Data.TypedResult

import           Control.Arrow

import           Text.Printf

data Term
  = Cons Constructor [Term] Sort
  | StringLiteral Text
  | NumberLiteral Int
  deriving (Eq)
 
type TermEnv = ConcreteTermEnv Term

evalModule :: Module -> Strat -> (Term,TermEnv) -> TypedResult (Term,TermEnv)
evalModule module_ = eval (signature module_) (stratEnv module_)

eval :: Signature -> StratEnv -> Strat -> (Term,TermEnv) -> TypedResult (Term,TermEnv)
eval sig senv s = runInterp (eval' s) (sig, senv)

instance HasTerm Term (Interp (Signature,senv) s TypedResult) where
  matchTerm = arr $ \t -> case t of
    Cons c ts _ -> T.Cons c ts
    StringLiteral s -> T.StringLiteral s
    NumberLiteral n -> T.NumberLiteral n
  {-# INLINE matchTerm #-}

  term = proc t0 -> case t0 of
    T.Cons "Cons" [x,xs] -> do
      t' <- case getSort xs of
              List t -> lub -< (getSort x,t)
              _ -> typeError -< "tail of the list is not of type list"
      xs' <- updateTag -< (xs,List t')
      returnA -< Cons "Cons" [x, xs'] (List t')
    T.Cons "Nil" [] ->
      returnA -< Cons "Nil" [] $ List Bottom
    T.Cons "Some" [x] ->
      returnA -< Cons "Some" [x] $ Option $ getSort x
    T.Cons "None" [] ->
     returnA -< Cons "None" [] $ Option Bottom
    T.Cons "" ts -> returnA -< Cons "" ts $ Tuple $ map getSort ts
    T.Cons c ts -> do
      sig <- getSignature -< ()
      case Sig.lookupType c sig of
        Just (Fun ss rs)
          | eqLength ss ts ->
              if and (zipWith (subtype sig) (map getSort ts) ss) 
              then returnA -< Cons c ts rs
              else typeError -< pack $ printf "constructor application not well typed: %s\nexpected arguments: %s\nbut got: %s" (show c) (show ss) (show (map getSort ts))
          | otherwise -> typeError -< pack $ "Wrong number of arguments to constructor: " ++ show c
        Nothing -> typeError -< pack $ "cannot find constructor: " ++ show c
    T.StringLiteral s -> returnA -< StringLiteral s
    T.NumberLiteral n -> returnA -< NumberLiteral n
    _ -> returnA -< error "Pattern match non exhaustive"

updateTag :: (HasSignature p, TypeError p,ArrowChoice p) => p (Term, Sort) Term
updateTag = proc x0 -> case x0 of
  (Cons "Some" [x] _, Option s) -> do
    x' <- updateTag -< (x,s)
    returnA -< Cons "Some" [x'] (Option s)
  (Cons "None" [] _, Option s) ->
    returnA -< Cons "None" [] (Option s)
  (Cons "Cons" [x,xs] _, List s) -> do
    x'  <- updateTag -< (x,s)
    xs' <- updateTag -< (xs,List s)
    returnA -< Cons "Cons" [x', xs'] (List s)
  (Cons "Nil" [] _, List s) -> returnA -< Cons "Nil" [] (List s)
  (Cons "" xs _, Tuple ss) -> do
    xs' <- zipWithA updateTag -< (xs,ss)
    returnA -< Cons "" xs' (Tuple ss)
  (t, s') -> do
    sig <- getSignature -< ()
    if Sig.subtype sig (getSort t) s'
      then returnA -< t
      else typeError -< pack $ printf "Expected term of sort %s, but got %s" (show s') (show (getSort t))
       
lub :: (Arrow p, HasSignature p) => p (Sort,Sort) Sort
lub = proc (s1,s2) -> do
  sig <- getSignature -< ()
  returnA -< Sig.lub sig s1 s2

instance Show Term where
  show (Cons c ts s) = show c ++ (if null ts then "" else show ts) ++ ":" ++ show s
  show (StringLiteral s) = show s
  show (NumberLiteral n) = show n

getSort :: Term -> Sort
getSort t = case t of
  Cons _ _ s -> s
  StringLiteral _ -> Sort "String"
  NumberLiteral _ -> Sort "INT"

instance PreOrd Term where
  Cons c ts _ ⊑ Cons c' ts' _ = c == c' && ts ⊑ ts'
  StringLiteral s ⊑ StringLiteral s' = s == s'
  NumberLiteral n ⊑ NumberLiteral n' = n == n'
  _ ⊑ _ = False

instance PartOrd Term

instance Lattice Term where
  Cons c ts t ⊔ Cons c' ts' _
    | c == c' && eqLength ts ts' = Cons c (zipWith (⊔) ts ts') t
    | otherwise = error "Top"
  StringLiteral s ⊔ StringLiteral s'
    | s == s' = StringLiteral s
    | otherwise = error "Top"
  NumberLiteral n ⊔ NumberLiteral n'
    | n == n' = NumberLiteral n
    | otherwise = error "Top"
  _ ⊔ _ = error "Top"
