{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
module TypedSemantics where

import           Prelude hiding (id,(.),fail,all)

import           ConcreteSemantics hiding (Term(..),TermEnv)
import           Constructor
import           Interpreter
import           Signature
import           Result
import           Term(HasTerm(..))
import qualified Term as T
import           Syntax(Strat,StratEnv,TermVar)

import           Data.Maybe
import           Data.Hashable (Hashable)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as S
import           Data.HashMap.Lazy (HashMap)
import           Data.Text(Text)

import           Control.Category
import           Control.Arrow

data Term
  = Cons Constructor [Term] (HashSet Sort)
  | StringLiteral Text
  | NumberLiteral Int
  deriving (Eq)
 
type TermEnv = HashMap TermVar Term

eval :: Signature -> StratEnv -> Strat -> (Term,TermEnv) -> Result (Term,TermEnv)
eval sig senv s = runInterp (eval' s) (sig, senv)

instance HasTerm Term (Interp (Signature,StratEnv) s) where
  matchTerm = arr $ \t -> case t of
    Cons c ts _ -> T.Cons c ts
    StringLiteral s -> T.StringLiteral s
    NumberLiteral n -> T.NumberLiteral n
  {-# INLINE matchTerm #-}

  term = proc t0 -> case t0 of
    T.Cons "Cons" [x,xs] -> do
      ss <- lub -< (getSort x,mapMaybeSet getListType $ getSort xs)
      let ls = S.map List ss
      returnA -< Cons "Cons" [x `setSort` ss, xs `setSort` ls] ls
    T.Cons "Nil" [] ->
      returnA -< Cons "Nil" [] $ S.singleton $ List Bottom
    T.Cons "Some" [x] ->
      returnA -< Cons "Some" [x] $ S.map Option $ getSort x
    T.Cons "None" [] ->
      returnA -< Cons "None" [] $ S.singleton $ Option Bottom
    T.Cons c ts -> do
      sig <- getSignature -< ()
      case lookupType c sig of
        Just (Fun ss rs) | eqLength ss ts -> do
          _ -< _
        Nothing -> returnA -< error $ "cannot find constructor: " ++ show c
    T.StringLiteral s -> returnA -< StringLiteral s
    T.NumberLiteral n -> returnA -< NumberLiteral n
    _ -> returnA -< error $ "Term is not well sorted" ++ show t0
    where
      getListType :: Sort -> Maybe Sort
      getListType (List s) = Just s
      getListType _ = Nothing

      mapMaybeSet :: (Eq b, Hashable b) => (a -> Maybe b) -> HashSet a -> HashSet b
      mapMaybeSet f = S.fromList . mapMaybe f . S.toList

lub :: p (f Sort,f Sort) (f Sort)
lub = undefined

getSort :: Term -> HashSet Sort
getSort t = case t of
  Cons _ _ s -> s
  StringLiteral _ -> S.singleton $ Sort "String"
  NumberLiteral _ -> S.singleton $ Sort "INT"

setSort :: Term -> HashSet Sort -> Term
setSort t s = case t of
  Cons c ts _ -> Cons c ts s
  _ -> t

getSignature :: Interp (Signature,StratEnv) r () Signature
getSignature = Interp $ \(sig,_) (_,e) -> Success (sig,e)

instance HasTermEnv Term (Interp r TermEnv) where
  getTermEnv = Interp $ \_ (_,e) -> Success (e,e)
  {-# INLINE getTermEnv #-}
  putTermEnv = Interp $ \_ (e,_) -> Success ((),e)
  {-# INLINE putTermEnv #-}

instance HasStratEnv (Interp (Signature,StratEnv) s) where
  readStratEnv = Interp $ \(_,r) (_,e) -> Success (r,e)
  {-# INLINE readStratEnv #-}
  localStratEnv (Interp f) = Interp $ \(s,_) ((a,r),e) -> f (s,r) (a,e)
  {-# INLINE localStratEnv #-}

instance Monoid Term where
  mempty = undefined
  mappend = undefined

instance Show Term where
  show (Cons c ts s) = show c ++ if null ts then "" else show ts ++ ":" ++ show s
  show (StringLiteral s) = show s
  show (NumberLiteral n) = show n
