{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TypedSemantics where

import           Prelude hiding (id,(.),fail,all)

import           InterpreterArrow
import           ConcreteSemantics hiding (Term(..),TermEnv)
import           Sort
import           Signature hiding (lookupType)
import qualified Signature as Sig
import           Syntax(Strat,StratEnv,TermVar)
import           Utils

import           Data.Term(HasTerm(..))
import qualified Data.Term as T
import           Data.Constructor
import           Data.TypedResult
import           Data.Maybe
import           Data.HashMap.Lazy (HashMap)
import           Data.Text(Text,pack)

import           Control.Arrow

data Term
  = Cons Constructor [Term] [Sort]
  | StringLiteral Text
  | NumberLiteral Int
  deriving (Eq)
 
type TermEnv = HashMap TermVar Term

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
      ss <- lub -< getSort x ++ mapMaybe getListType (getSort xs)
      returnA -< Cons "Cons" [x, xs] $ List <$> ss
    T.Cons "Nil" [] ->
      returnA -< Cons "Nil" [] $ return $ List Bottom
    T.Cons "Some" [x] ->
      returnA -< Cons "Some" [x] $ Option <$> getSort x
    T.Cons "None" [] ->
     returnA -< Cons "None" [] $ return $ Option Bottom
    T.Cons c ts -> do
      sig <- getSignature -< ()
      case Sig.lookupType c sig of
        Just (Fun ss rs)
          | eqLength ss ts ->
              if null (zipWith (\xs y -> filter (\x -> subtype sig x y) xs) (map getSort ts) ss) 
              then typeError -< pack $ "constructor application not well typed: " ++ show c
              else returnA -< Cons c ts [rs]
          | otherwise -> typeError -< pack $ "Wrong number of arguments to constructor: " ++ show c
        Nothing -> typeError -< pack $ "cannot find constructor: " ++ show c
    T.StringLiteral s -> returnA -< StringLiteral s
    T.NumberLiteral n -> returnA -< NumberLiteral n
    _ -> returnA -< error "Pattern match non exhaustive"
    where
      nonEmpty :: Interp r s TypedResult Term Term
      nonEmpty = proc t -> case getSort t of
        [] -> typeError  -< "Term is not well sorted"
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
