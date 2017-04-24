{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
module RegularTreeGrammar where

import Syntax (Constructor)
import WildcardSemantics (Term)
import qualified WildcardSemantics as W

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Sequence (Seq)
import Data.Text (Text,unpack)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.Hashable (Hashable(..))

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer

data Symbol = Symbol {-# UNPACK #-} !Int {-# UNPACK #-} !Text 

instance Eq Symbol where
  Symbol i _ == Symbol j _ = i == j

instance Ord Symbol where
  Symbol i _ <= Symbol j _ = i <= j
  Symbol i _ `compare` Symbol j _ = i `compare` j

instance Show Symbol where
  show (Symbol _ t) = unpack t

instance Hashable Symbol where
  hashWithSalt s (Symbol i _) = s `hashWithSalt` i

instance Hashable GrammarTerm where
  hashWithSalt s (Cons c ts) = s `hashWithSalt` (0::Int) `hashWithSalt` c `hashWithSalt` ts
  hashWithSalt s AnyString = s `hashWithSalt` (1::Int)
  hashWithSalt s (StringLiteral t) = s `hashWithSalt` (2::Int) `hashWithSalt` t
  hashWithSalt s AnyNumber = s `hashWithSalt` (3::Int)
  hashWithSalt s (NumberLiteral n) = s `hashWithSalt` (4::Int) `hashWithSalt` n
  hashWithSalt s (NonTerminal n) = s `hashWithSalt` (5::Int) `hashWithSalt` n

data GrammarTerm
  = Cons Constructor [GrammarTerm]
  | AnyString
  | StringLiteral Text
  | AnyNumber
  | NumberLiteral Int
  | NonTerminal Symbol
  deriving (Eq)

data Rule = Rule {-# UNPACK #-} !Symbol {-# UNPACK #-} !GrammarTerm deriving Eq

instance Hashable Rule where
  hashWithSalt s (Rule sym term) = s `hashWithSalt` sym `hashWithSalt` term

type Grammar = Vector Rule

type Summary = HashMap Rule (Sum Int)

summary :: Functor f => Grammar -> f Term -> f (Maybe Summary)
summary g = fmap (fmap (foldMap ruleSummary) . runTopDown g)
  where
    ruleSummary :: Rule -> Summary
    ruleSummary rule = H.insert rule (Sum 1) initial

    initial :: Summary
    initial = H.fromList $ (,Sum 0) <$> V.toList g

runTopDown :: Grammar -> Term -> Maybe (Seq Rule)
runTopDown grammar term = execWriterT (runReaderT (topDown (Symbol 0 "") term) grammar)

topDown :: (MonadReader Grammar m, MonadWriter (Seq Rule) m, MonadPlus m) => Symbol -> Term -> m ()
topDown startSymbol term = do
  grammar <- ask
  go grammar
  where
    go :: (MonadReader Grammar m, MonadWriter (Seq Rule) m, MonadPlus m) => Grammar -> m ()
    go = V.foldr (\rule@(Rule symbol rhs) -> mplus (guard (startSymbol == symbol) >> match rhs term >> tell (return rule))) mzero

    match :: (MonadReader Grammar m, MonadWriter (Seq Rule) m, MonadPlus m) => GrammarTerm -> Term -> m ()
    match (Cons s ts) (W.Cons s' ts') | s == s' && length ts == length ts' = zipWithM_ match ts ts'

    match AnyString (W.StringLiteral _) = return ()
    match (StringLiteral s) (W.StringLiteral s') = guard $ s == s'

    match AnyNumber (W.NumberLiteral _) = return ()
    match (NumberLiteral n) (W.NumberLiteral n') = guard $ n == n'

    match _ W.Wildcard = return ()

    match (NonTerminal n) t = topDown n t

    match _ _ = mzero

{-
Start Symbol: NF

NF -> Zero
NF -> Succ(NF)
NF -> Pred(NF)
NF -> Abs(String,Type,Expr)

Expr -> Var(String)
Expr -> App(Expr,Expr)
Expr -> Abs(String,Type,Expr)
Expr -> Zero
Expr -> Succ(Expr)
Expr -> Pred(Expr)
Expr -> Ifz(Expr,Expr,Expr)

Type -> Num()
Type -> Fun(Type,Type)
-}
pcfGrammar :: Grammar
pcfGrammar =
  V.fromList [
    Rule nf $ Cons "Zero" [],
    Rule nf $ Cons "Succ" [nf'],
    Rule nf $ Cons "Pred" [nf'],
    Rule nf $ Cons "Abs" [AnyString, typ', expr'],

    Rule expr $ Cons "Var" [AnyString],
    Rule expr $ Cons "App" [expr',expr'],
    Rule expr $ Cons "Abs" [AnyString, typ', expr'],
    Rule expr $ Cons "Zero" [],
    Rule expr $ Cons "Succ" [expr'],
    Rule expr $ Cons "Pred" [expr'],
    Rule expr $ Cons "Ifz" [expr', expr', expr'],

    Rule typ $ Cons "Num" [],
    Rule typ $ Cons "Fun" [typ', typ']
  ]
  where
    nf = Symbol 0 "NF"
    nf' = NonTerminal nf
    expr = Symbol 1 "Expr"
    expr' = NonTerminal expr
    typ = Symbol 2 "Type"
    typ' = NonTerminal typ
