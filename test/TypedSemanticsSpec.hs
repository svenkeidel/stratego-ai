{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module TypedSemanticsSpec where

import           Prelude hiding (succ,abs)

import           TypedSemantics
import           Sort
import           Signature (Signature)
import qualified Signature as S
import           InterpreterArrow
import           Paths_system_s
import           Syntax(parseModule,Strat(..))
import           Utils

import           Data.ATerm(parseATerm)
import           Data.TypedResult
import qualified Data.Term as T
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Lazy as M

import           Test.Hspec


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  
  let sig = S.empty & S.insertSubtype "String" "Expr"
                    & S.insertSubtype "INT" "Expr"
                    & S.insertType "Add" (S.Fun ["Expr", "Expr"] "Expr")

  it "lists should belong to the correct sort" $ do
    term' S.empty (T.Cons "Nil" []) `shouldBe` Success nil
    term' S.empty (T.Cons "Cons" [StringLiteral "foo", StringLiteral "bar"]) `shouldBe`
      TypeError "tail of the list is not of type list"

    term' S.empty (T.Cons "Cons" [StringLiteral "foo", nil]) `shouldBe`
      Success (cons (StringLiteral "foo") nil [List "String"])

    term' sig (T.Cons "Cons" [StringLiteral "foo", nil]) `shouldBe`
      Success (cons (StringLiteral "foo") nil [List "String"])

    term' sig (T.Cons "Cons" [NumberLiteral 1, cons (StringLiteral "foo") nil [List "String"]]) `shouldBe`
      Success (cons (NumberLiteral 1) (cons (StringLiteral "foo") nil [List "String"]) [List "Expr"])

    term' S.empty (T.Cons "Cons" [NumberLiteral 1, cons (StringLiteral "foo") nil [List "String"]]) `shouldBe`
      TypeError "there exists no valid typing for the term"

  it "options should belong to the correct sort" $ do
    term' sig (T.Cons "None" []) `shouldBe` Success none
    term' sig (T.Cons "Some" [StringLiteral "foo"]) `shouldBe`
      Success (Cons "Some" [StringLiteral "foo"] [Option "String"])
  
  it "constructors should belong to the correct sort" $ do
    term' sig (T.Cons "Add" [NumberLiteral 1, StringLiteral "foo"]) `shouldBe`
      Success (Cons "Add" [NumberLiteral 1, StringLiteral "foo"] ["Expr"])
    term' sig (T.Cons "Add" [nil, StringLiteral "foo"]) `shouldBe`
      TypeError "constructor application not well typed: Add\nexpected arguments: [Expr,Expr]\nbut got: [[List (Bottom)],[String]]"

  it "tuples should be well sorted" $ do
    term' sig (T.Cons "" [NumberLiteral 1, StringLiteral "foo"]) `shouldBe`
      Success (Cons "" [NumberLiteral 1, StringLiteral "foo"] [Tuple ["INT","String"]])

    term' sig (T.Cons "" []) `shouldBe` Success (Cons "" [] [Tuple []])
 
  describe "Case Studies" $ describe "PCF" $ beforeAll parsePCFCaseStudy $
    it "should execute well typed programs without a problem" $ \m -> do
      evalPCF m (tup [nil, zero])
        `shouldBe` Success (zero, M.empty)

      evalPCF m (tup [nil, abs "x" num (var "x")])
        `shouldBe` Success (abs "x" num (var "x"), M.empty)
 
      evalPCF m (tup [nil, app (abs "x" num (succ (var "x"))) zero])
        `shouldBe` Success (succ zero, M.empty)

  where
    
    evalPCF module_ t = evalModule module_ (Call "eval_0_0" [] []) (t,M.empty)

    term' :: Signature -> T.TermF Term -> TypedResult Term
    term' sig t = fst <$> runInterp T.term (sig,()) (t,())

    cons :: Term -> Term -> [Sort] -> Term
    cons x xs = Cons "Cons" [x,xs]

    nil :: Term
    nil = Cons "Nil" [] [List Bottom]

    none :: Term
    none = Cons "None" [] [Option Bottom]

    (&) = flip ($)

    parsePCFCaseStudy = do
      file <- TIO.readFile =<< getDataFileName "case-studies/pcf/pcf.aterm"
      case parseModule =<< parseATerm file of
        Left e -> fail (show e)
        Right module_ -> return module_

    zero :: Term
    zero = Cons "Zero" [] ["Exp"]

    succ :: Term -> Term
    succ e = Cons "Succ" [e] ["Exp"]

    var :: Text -> Term
    var x = Cons "Var" [StringLiteral x] ["Exp"]

    app :: Term -> Term -> Term
    app e1 e2 = Cons "App" [e1,e2] ["Exp"]

    abs :: Text -> Term -> Term -> Term
    abs x t e = Cons "Abs" [StringLiteral x, t, e] ["Exp"]

    num :: Term
    num = Cons "Num" [] ["Type"]

    tup :: [Term] -> Term
    tup ts = Cons "" ts (Tuple <$> permutations (map getSort ts))
