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
import           Signature hiding (lookupType,lub)
import qualified Signature as Sig
import           Syntax(Strat,StratEnv,TermVar,Module,signature,stratEnv)
import           Utils

import           Control.Arrow hiding ((<+>))
import           Control.Arrow.Append
import           Control.Arrow.Try

import           Data.Constructor
import           Data.HashMap.Lazy (HashMap)
import           Data.Hashable
import           Data.Order hiding (lub)
import           Data.PowersetResult
import qualified Data.Term as T
import           Data.Term(HasTerm(..),TermF)
import           Data.Text(Text,pack)
import           Data.TypedResult(TypeError(..))

import           Text.Printf

data Term
  = Cons Constructor [Term] Sort
  | StringLiteral Text
  | NumberLiteral Int
  | Wildcard Sort
  deriving (Eq)

type TermEnv = HashMap TermVar Term

evalModule :: Int -> Module -> Strat -> (Term,TermEnv) -> PowersetResult (Term,TermEnv)
evalModule i module_ = eval i (signature module_) (stratEnv module_)

eval :: Int -> Signature -> StratEnv -> Strat -> (Term,TermEnv) -> PowersetResult (Term,TermEnv)
eval i sig senv s = runInterp (eval' i s) (sig, senv)

instance HasTerm Term (Interp (Signature,senv) s PowersetResult) where
  matchTerm = proc t -> case t of
    Cons c ts _ -> returnA -< T.Cons c ts
    StringLiteral s -> returnA -< T.StringLiteral s
    NumberLiteral n -> returnA -< T.NumberLiteral n
    Wildcard Top -> returnA -< T.Wildcard
    Wildcard (Option s) ->
      alternatives -< [T.Cons "None" [], T.Cons "Some" [Wildcard s]]
    Wildcard (List s) ->
      alternatives -< [T.Cons "Nil" [], T.Cons "Cons" [Wildcard s, Wildcard (List s)]]
    Wildcard (Tuple ts) ->
      alternatives -< [T.Cons "" [ Wildcard s | s <- ts ]]
    Wildcard (Coproduct s1 s2) ->
      (matchTerm -< Wildcard s1) <+> (matchTerm -< Wildcard s2)
    Wildcard sort -> do
      sig <- getSignature -< ()
      alternatives -< do
        (c,Fun args r) <- inhabitants sig sort
        case r of
          "String" -> return T.Wildcard
          "INT" -> return T.Wildcard
          _ -> return $ T.Cons c [ Wildcard s | s <- args ]
  {-# INLINE matchTerm #-}

  matchTermAgainstConstructor = proc (c,t) -> case t of
    Cons c' ts _ | c' == c -> returnA -< T.Cons c ts
                 | otherwise -> fail -< ()
    StringLiteral s -> returnA -< T.StringLiteral s
    NumberLiteral n -> returnA -< T.NumberLiteral n
    Wildcard Top -> returnA -< T.Wildcard
    Wildcard (Option s)
      | c == "Some" -> returnA -< T.Cons "Some" [Wildcard s]
      | c == "None" -> returnA -< T.Cons "None" []
      | otherwise -> fail -< ()
    Wildcard (List s)
      | c == "Cons" -> returnA -< T.Cons "Cons" [Wildcard s, Wildcard (List s)]
      | c == "Nil" -> returnA -< T.Cons "Nil" []
      | otherwise -> fail -< ()
    Wildcard (Tuple ts)
      | c == "" -> returnA -< T.Cons "" [ Wildcard s | s <- ts ]
      | otherwise -> fail -< ()
    Wildcard (Coproduct s1 s2) ->
      (matchTermAgainstConstructor -< (c,Wildcard s1)) <+> (matchTermAgainstConstructor -< (c,Wildcard s2))
    Wildcard _ -> do
      sig <- getSignature -< ()
      case Sig.lookupType c sig of
        Just (Fun args _) -> fail <+> returnA -< T.Cons c [ Wildcard s | s <- args ]
        Nothing -> typeError -< pack $ "cannot find constructor: " ++ show c
  {-# INLINE matchTermAgainstConstructor #-}

  term = proc t0 -> case t0 of
    T.Cons "Cons" [x,xs] -> do
      t' <- case getSort xs of
              List t -> lub -< (getSort x,t)
              Top -> returnA -< getSort x
              _ -> typeError -< "tail of the list is not of type list"
      xs' <- if List t' /= getSort xs
               then updateTag -< (xs,List t')
               else returnA -< xs
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
          | eqLength ss ts -> do
              ts' <- zipWithA updateTag -< (ts,ss)
              returnA -< Cons c ts' rs
          | otherwise -> typeError -< pack $ "Wrong number of arguments to constructor: " ++ show c
        Nothing -> typeError -< pack $ "cannot find constructor: " ++ show c
    T.StringLiteral s -> returnA -< StringLiteral s
    T.NumberLiteral n -> returnA -< NumberLiteral n
    T.Wildcard -> returnA -< Wildcard Top
    _ -> returnA -< error "Pattern match non exhaustive"

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
 

-- updates all type tags of terms of structural types while checking
-- that all terms of non-structural types are subtypes of the expected type.
updateTag :: (HasSignature p, TypeError p,ArrowChoice p) => p (Term, Sort) Term
updateTag = proc x0 -> case x0 of
  (Wildcard s',s) -> do
    sig <- getSignature -< ()
    case () of
      _ | Sig.subtype sig s s' || Sig.subtype sig s' s -> returnA -< Wildcard s
        | otherwise -> typeError -< pack $ printf "Cannot downcast wildcard from %s to %s" (show s') (show s)
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
    
getSort :: Term -> Sort
getSort t = case t of
  Cons _ _ s -> s
  StringLiteral _ -> Sort "String"
  NumberLiteral _ -> Sort "INT"
  Wildcard s -> s

lub :: HasSignature p => p (Sort,Sort) Sort
lub = proc (s1,s2) -> do
  sig <- getSignature -< ()
  returnA -< Sig.lub sig s1 s2

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

instance PreOrd Term where
  t ⊑ Wildcard s' = subtype sig (getSort t) s'
  Cons c ts _ ⊑ Cons c' ts' _ = c == c' && ts ⊑ ts'
  StringLiteral s ⊑ StringLiteral s' = s == s'
  NumberLiteral n ⊑ NumberLiteral n' = n == n'
  _ ⊑ _ = False

