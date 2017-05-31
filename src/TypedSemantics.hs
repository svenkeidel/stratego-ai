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
import           Signature hiding (lookupType)
import qualified Signature as Sig
import           Syntax(Module,Strat,StratEnv,TermVar,stratEnv,signature)
import           Utils

import           Data.Term(HasTerm(..))
import qualified Data.Term as T
import           Data.Constructor
import           Data.TypedResult
import           Data.Maybe
import           Data.HashMap.Lazy (HashMap)
import           Data.Text(Text,pack)

import           Control.Arrow

import           Text.Printf

data Term
  = Cons Constructor [Term] [Sort]
  | StringLiteral Text
  | NumberLiteral Int
  deriving (Eq)
 
type TermEnv = HashMap TermVar Term

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

  term = nonEmpty <<< proc t0 -> case t0 of
    T.Cons "Cons" [x,xs] -> do
      let listTypes = mapMaybe getListType (getSort xs) 
      if null listTypes
        then typeError -< "tail of the list is not of type list"
        else do
          ss <- lub -< getSort x ++ listTypes
          returnA -< refineListSort (Cons "Cons" [x, xs] []) [List s | s <- ss]
    T.Cons "Nil" [] ->
      returnA -< Cons "Nil" [] $ return $ List Bottom
    T.Cons "Some" [x] ->
      returnA -< Cons "Some" [x] $ Option <$> getSort x
    T.Cons "None" [] ->
     returnA -< Cons "None" [] $ return $ Option Bottom
    T.Cons "" ts -> returnA -< Cons "" ts (Tuple <$> permutations (map getSort ts))
    T.Cons c ts -> do
      sig <- getSignature -< ()
      case Sig.lookupType c sig of
        Just (Fun ss rs)
          | eqLength ss ts ->
              if and (zipWith (\xs y -> any (\x -> subtype sig x y) xs) (map getSort ts) ss) 
              then returnA -< Cons c ts [rs]
              else typeError -< pack $ printf "constructor application not well typed: %s\nexpected arguments: %s\nbut got: %s" (show c) (show ss) (show (map getSort ts))
          | otherwise -> typeError -< pack $ "Wrong number of arguments to constructor: " ++ show c
        Nothing -> typeError -< pack $ "cannot find constructor: " ++ show c
    T.StringLiteral s -> returnA -< StringLiteral s
    T.NumberLiteral n -> returnA -< NumberLiteral n
    _ -> returnA -< error "Pattern match non exhaustive"
    where
      nonEmpty :: Interp r s TypedResult Term Term
      nonEmpty = proc t -> case getSort t of
        [] -> typeError  -< "there exists no valid typing for the term"
        _ -> returnA -< t

instance Monoid Term where
  mempty = undefined
  mappend = undefined

instance Show Term where
  show (Cons c ts s) = show c ++ if null ts then "" else show ts ++ ":" ++ show s
  show (StringLiteral s) = show s
  show (NumberLiteral n) = show n

lub :: HasSignature p => p [Sort] [Sort]
lub = proc l -> do
  sig <- getSignature -< ()
  returnA -< lubs sig l

getSort :: Term -> [Sort]
getSort t = case t of
  Cons _ _ s -> s
  StringLiteral _ -> return $  Sort "String"
  NumberLiteral _ -> return $  Sort "INT"

typeError :: Interp r s TypedResult Text a
typeError = Interp $ \_ (e,_) -> TypeError e
