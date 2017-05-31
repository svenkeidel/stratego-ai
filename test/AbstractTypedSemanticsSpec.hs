{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module AbstractTypedSemanticsSpec where

import           AbstractTypedSemantics
import           Sort
import           Signature (Signature)
import qualified Signature as S
import           InterpreterArrow
import           Paths_system_s
import           Syntax(parseModule,Strat(..))
import           Utils

import           Data.ATerm(parseATerm)
import           Data.PowersetResult
import           Data.Result
import qualified Data.Term as T
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Lazy as M
import           Data.Foldable

import           Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  
  let sig = S.empty & S.insertSubtype "String" "Expr"
                    & S.insertSubtype "INT" "Expr"
                    & S.insertType "Add" (S.Fun ["Expr", "Expr"] "Expr")

  it "lists should belong to the correct sort" $ do
    term' S.empty (T.Cons "Nil" []) `shouldMatchList` [Success nil]

    term' S.empty (T.Cons "Cons" [StringLiteral "foo", StringLiteral "bar"])
      `shouldMatchList` []

    term' S.empty (T.Cons "Cons" [StringLiteral "foo", nil])
      `shouldMatchList` [Success (cons (StringLiteral "foo") nil [List "String"])]

    term' sig (T.Cons "Cons" [StringLiteral "foo", nil])
      `shouldMatchList` [Success (cons (StringLiteral "foo") nil [List "String"])]

    term' sig (T.Cons "Cons" [NumberLiteral 1, cons (StringLiteral "foo") nil [List "String"]]) `shouldMatchList`
      [Success (cons (NumberLiteral 1) (cons (StringLiteral "foo") nil [List "String"]) [List "Expr"])]

    term' S.empty (T.Cons "Cons" [NumberLiteral 1, cons (StringLiteral "foo") nil [List "String"]])
      `shouldMatchList` []
    
    term' sig (T.Cons "Cons" [Wildcard [Top], Wildcard [Top]])
      `shouldMatchList` [Success (cons (Wildcard [Top]) (Wildcard [List Top]) [List Top])]

    term' sig (T.Cons "Cons" [Wildcard ["String"], Wildcard [Top]])
      `shouldMatchList` [Success (cons (Wildcard ["String"]) (Wildcard [List "String"]) [List "String"])]

    term' sig (T.Cons "Cons" [Wildcard [Top], Wildcard [List "String"]])
      `shouldMatchList` [Success (cons (Wildcard ["String"]) (Wildcard [List "String"]) [List "String"])]

    term' sig (T.Cons "Cons" [Wildcard [Top], nil])
      `shouldMatchList` [Success (cons (Wildcard [Top]) nil [List Top])]

    term' sig (T.Cons "Cons" [NumberLiteral 1, cons (Wildcard [Top]) nil [List Top]])
      `shouldMatchList` [Success (cons (NumberLiteral 1) (cons (Wildcard ["INT"]) nil [List "INT"]) [List "INT"])]

    term' sig (T.Cons "Cons" [some (Wildcard [Top]) [Option Top], nil])
      `shouldMatchList` [Success (cons (some (Wildcard [Top]) [Option Top]) nil [List (Option Top)])]

    term' sig (T.Cons "Cons" [some (NumberLiteral 1) [Option "INT"], cons (some (Wildcard [Top]) [Option Top]) nil [List (Option Top)]])
      `shouldMatchList` [Success (cons
                                  (some (NumberLiteral 1) [Option "INT"])
                                  (cons
                                   (some (Wildcard ["INT"]) [Option "INT"])
                                   nil [List (Option "INT")])
                                 [List (Option "INT")])]

  where 
    term' :: Signature -> T.TermF Term -> [Result Term]
    term' sig t = fmap fst <$> toList (unPowRes (runInterp T.term (sig,()) (t,())))

    cons :: Term -> Term -> [Sort] -> Term
    cons x xs = Cons "Cons" [x,xs]

    some :: Term -> [Sort] -> Term
    some x = Cons "Some" [x]

    nil :: Term
    nil = Cons "Nil" [] [List Bottom]

    none :: Term
    none = Cons "None" [] [Option Bottom]

    (&) = flip ($)
