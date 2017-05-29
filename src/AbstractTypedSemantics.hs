{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import           Control.Arrow
import           Control.Arrow.Append


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
    Wildcard [] -> returnA -< T.Wildcard
    Wildcard sorts -> do
      sig <- getSignature -< ()
      alternatives -< do
        sort <- sorts
        (c,Fun args _) <- inhabitants sig sort
        return $ T.Cons c [ Wildcard [s] | s <- args ]
  {-# INLINE matchTerm #-}

  term = _ -- nonEmpty <<< proc t0 -> case t0 of
    -- T.Cons "Cons" [x,xs] -> do
    --   ss <- lub -< getSort x ++ mapMaybe getListType (getSort xs)
    --   returnA -< Cons "Cons" [x, xs] $ List <$> ss
    -- T.Cons "Nil" [] ->
    --   returnA -< Cons "Nil" [] $ return $ List Bottom
    -- T.Cons "Some" [x] ->
    --   returnA -< Cons "Some" [x] $ Option <$> getSort x
    -- T.Cons "None" [] ->
    --  returnA -< Cons "None" [] $ return $ Option Bottom
    -- T.Cons c ts -> do
    --   sig <- getSignature -< ()
    --   case Sig.lookupType c sig of
    --     Just (Fun ss rs)
    --       | eqLength ss ts ->
    --           if null (zipWith (\xs y -> filter (\x -> subtype sig x y) xs) (map getSort ts) ss) 
    --           then typeError -< pack $ "constructor application not well typed: " ++ show c
    --           else returnA -< Cons c ts [rs]
    --       | otherwise -> typeError -< pack $ "Wrong number of arguments to constructor: " ++ show c
    --     Nothing -> typeError -< pack $ "cannot find constructor: " ++ show c
    -- T.StringLiteral s -> returnA -< StringLiteral s
    -- T.NumberLiteral n -> returnA -< NumberLiteral n
    -- _ -> returnA -< error "Pattern match non exhaustive"
    -- where
    --   nonEmpty :: Interp r s PowersetResult Term Term
    --   nonEmpty = proc t -> case getSort t of
    --     [] -> typeError  -< "Term is not well sorted"
    --     _ -> returnA -< t

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
