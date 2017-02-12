{-# LANGUAGE OverloadedStrings #-}
module HaskellPretty where

import           WildcardSemantics
import           Result

import           Data.Foldable(toList)
import           Data.Sequence (Seq)
import qualified Data.Sequence as S

import           Text.PrettyPrint hiding (sep)


ppResults :: Seq (Result (Term,TermEnv)) -> Doc
ppResults res = braces
              $ cat
              $ punctuate (comma <> space)
              $ toList
              $ fmap ppResult
              $ S.reverse
              $ S.sort 
              $ (fmap.fmap) fst res

ppResult :: Result Term -> Doc
ppResult res = case res of
  Success t -> ppHaskell t
  Fail -> text "Fail"

ppHaskell :: Term -> Doc
ppHaskell t = case t of
  Cons "OpApp" [l,Cons o [],r] ->
    ppHaskell l <> space <> text (show o) <> space <> ppHaskell r
  Cons "AppBin" [f,x] ->
    ppHaskell f <> space <> ppHaskell x
  Cons "Var" [Cons x []] -> text (show x)
  Cons "Abs" [args, body] ->
    parens $ char '\\' <> ppSepList space args <> text " -> " <> ppHaskell body
  Cons "Let" [bnds, body] ->
    text "let " <> braces (ppLetBindings bnds) <>
    text " in " <> ppHaskell body
  Cons "If" [e1,e2,e3] ->
    text "if " <> ppHaskell e1 <>
    text " then " <> ppHaskell e2 <>
    text " else " <> ppHaskell e3
  Cons "Constr" [Cons t' []]
    | t' == "Unit" -> text "()"
    | otherwise    -> text (show t')
  Cons "Product" [t'] -> parens (ppSepList comma t')
  Cons "Tuple" [t1,t2] -> parens (ppHaskell t1 <> comma <> space <> ppSepList (comma <> space) t2)
  Wildcard -> ppWildcard
  _ -> error $ "unexpected term: " ++ show t

ppLetBindings :: Term -> Doc
ppLetBindings t = case t of
  Wildcard -> ppWildcard
  _ -> error $ "unexpected term: " ++ show t

ppSepList :: Doc -> Term -> Doc
ppSepList sep t = case t of
  Cons "ECons" [x, Cons "Nil" []] -> ppHaskell x
  Cons "ECons" [x, xs] -> ppHaskell x <> sep <> ppSepList sep xs
  Cons "Cons" [x, Cons "Nil" []] -> ppHaskell x
  Cons "Cons" [x, xs] -> ppHaskell x <> sep <> ppSepList sep xs
  Cons "Nil" [] -> mempty
  Wildcard -> ppWildcard
  _ -> error $ "unexpected term: " ++ show t

ppWildcard :: Doc
ppWildcard = char '*'
