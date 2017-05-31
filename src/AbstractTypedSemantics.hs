{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
module AbstractTypedSemantics where

import           Prelude hiding (id,(.),fail,all)

import           InterpreterArrow
import           WildcardSemantics hiding (Term(..),TermEnv)
import           Sort
import           Signature hiding (lookupType)
import qualified Signature as Sig
import           Syntax(Strat,StratEnv,TermVar)
import           Utils

import           Data.Term(HasTerm(..),TermF)
import qualified Data.Term as T
import           Data.Constructor
import           Data.PowersetResult
import           Data.Maybe
import           Data.HashMap.Lazy (HashMap)
import           Data.Hashable
import           Data.Text(Text,pack)
import           Data.List (transpose)

import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Append
import           Control.Arrow.Try

data Term
  = Cons Constructor [Term] [Sort]
  | StringLiteral Text
  | NumberLiteral Int
  | Wildcard [Sort]
  deriving (Eq)

type TermEnv = HashMap TermVar Term

eval :: Int -> Signature -> StratEnv -> Strat -> (Term,TermEnv) -> PowersetResult (Term,TermEnv)
eval i sig senv s = runInterp (eval' i s) (sig, senv)

instance HasTerm Term (Interp (Signature,senv) s PowersetResult) where
  matchTerm = proc t -> case t of
    Cons c ts _ -> returnA -< T.Cons c ts
    StringLiteral s -> returnA -< T.StringLiteral s
    NumberLiteral n -> returnA -< T.NumberLiteral n
    Wildcard [Top] -> returnA -< T.Wildcard
    Wildcard sorts@(Option _:_) ->
      alternatives -< T.Cons "None" [] : [T.Cons "Some" [Wildcard [s]] | Option s <- sorts]
    Wildcard sorts@(List _:_) ->
      alternatives -< T.Cons "Nil" [] : [T.Cons "Cons" [Wildcard [s], Wildcard [List s]] | List s <- sorts ]
    Wildcard sorts@(Tuple _:_) ->
      alternatives -< [T.Cons "" [ Wildcard [s] | s <- ts ] | Tuple ts <- sorts ]
    Wildcard sorts -> do
      sig <- getSignature -< ()
      alternatives -< do
        sort <- sorts
        (c,Fun args _) <- inhabitants sig sort
        return $ T.Cons c [ Wildcard [s] | s <- args ]
  {-# INLINE matchTerm #-}

  matchTermAgainstConstructor = proc (c,t) -> case t of
    Cons c' ts _ | c' == c -> returnA -< T.Cons c ts
                 | otherwise -> fail -< ()
    StringLiteral s -> returnA -< T.StringLiteral s
    NumberLiteral n -> returnA -< T.NumberLiteral n
    Wildcard _ -> do
      sig <- getSignature -< ()
      case Sig.lookupType c sig of
        Just (Fun args _) -> fail <+> returnA -< T.Cons c [ Wildcard [s] | s <- args ]
        Nothing -> typeError -< pack $ "cannot find constructor: " ++ show c
  {-# INLINE matchTermAgainstConstructor #-}

  equal = proc (t1,t2) -> case (t1,t2) of
    (Cons c ts tau,Cons c' ts' _)
      | c == c' && eqLength ts ts' -> do
        ts'' <- zipWithA equal -< (ts,ts')
        returnA -< Cons c ts'' tau
      | otherwise -> fail -< ()
    (StringLiteral s, StringLiteral s')
      | s == s' -> success -< t1
      | otherwise -> fail -< ()
    (NumberLiteral n, NumberLiteral n')
      | n == n' -> success -< t1
      | otherwise -> fail -< ()
    (Wildcard _, t) -> fail <+> returnA -< t
    (t, Wildcard _) -> fail <+> returnA -< t
    (_,_) -> fail -< ()
 

  term = nonEmpty <<< proc t0 -> case t0 of
    T.Cons "Cons" [x,Cons "Nil" [] _] -> returnA -< Cons "Cons" [x, Cons "Nil" [] [List Bottom]] [List s | s <- getSort x]
    T.Cons "Cons" [x,xs] -> do
        let (sortX, sortXS) = case (any containsTop (getSort x), any containsTop (getSort xs)) of
              (True,True) -> ([Top],[List Top])
              (True,_) -> ([s | List s <- getSort xs],getSort xs)
              (_,True) -> (getSort x,[List s | s <- getSort x])
              (_,_) -> (getSort x,getSort xs)
            listTypes = mapMaybe getListType sortXS
        if null listTypes
          then typeError -< pack $ "tail of the list is not of type list: " ++ show xs
          else do
            ss <- lub -< (sortX ++ listTypes)
            returnA -< Cons "Cons" [downcast x ss, downcast xs [List s | s <- ss]] [List s | s <- ss]
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
              if and (zipWith (\xs y -> any containsTop xs || any (\x -> subtype sig x y) xs) (map getSort ts) ss)
                then typeError -< pack $ "constructor application not well typed: " ++ show c
                else returnA -< Cons c (zipWith (\t s -> downcast t [s]) ts ss) [rs]
          | otherwise -> typeError -< pack $ "Wrong number of arguments to constructor: " ++ show c
        Nothing -> typeError -< pack $ "cannot find constructor: " ++ show c
    T.StringLiteral s -> returnA -< StringLiteral s
    T.NumberLiteral n -> returnA -< NumberLiteral n
    T.Wildcard -> returnA -< Wildcard [Top]
    _ -> returnA -< error "Pattern match non exhaustive"
    where
      nonEmpty :: Interp r s PowersetResult Term Term
      nonEmpty = proc t -> case getSort t of
        [] -> typeError  -< "Term is not well sorted"
        _ -> returnA -< t

downcast :: Term -> [Sort] -> Term
downcast t0 ss = case t0 of
  Wildcard _ -> Wildcard ss
  Cons "Some" [t] _ -> let t' = downcast t [s | Option s <- ss] in Cons "Some" [t'] [Option s | s <- (getSort t')]
  Cons "Cons" [x,xs] _ -> Cons "Cons" [downcast x [s | List s <- ss], downcast xs ss] ss
  Cons "" xs _ -> Cons "" (zipWith downcast xs (transpose [ts | Tuple ts <- ss])) ss
  t -> t

typeError :: Interp r s PowersetResult Text a
typeError = Interp $ \_ _ -> mempty

getSort :: Term -> [Sort]
getSort t = case t of
  Cons _ _ s -> s
  StringLiteral _ -> return $  Sort "String"
  NumberLiteral _ -> return $  Sort "INT"
  Wildcard s -> s

lub :: HasSignature p => p [Sort] [Sort]
lub = proc l -> do
  sig <- getSignature -< ()
  returnA -< lubs sig l

instance Show Term where
  show (Cons c ts s) = show c ++ (if null ts then "" else show ts) ++ ":" ++ show s
  show (StringLiteral s) = show s
  show (NumberLiteral n) = show n
  show (Wildcard s) = "Wildcard:" ++ show s

instance Hashable Term where
  hashWithSalt s (Cons c ts ss) = s `hashWithSalt` (0::Int) `hashWithSalt` c `hashWithSalt` ts `hashWithSalt` ss
  hashWithSalt s (StringLiteral t) = s `hashWithSalt` (1::Int) `hashWithSalt` t
  hashWithSalt s (NumberLiteral n) = s `hashWithSalt` (2::Int) `hashWithSalt` n
  hashWithSalt s (Wildcard ss) = s `hashWithSalt` (3::Int) `hashWithSalt` ss

instance Monoid Term where
  mempty = undefined -- Wildcard
  mappend = undefined --(<>)

instance Monoid (TermF Term) where
  mempty = undefined -- Wildcard
  mappend = undefined --(<>)
