{-# LANGUAGE OverloadedStrings #-}
module CaseStudiesSpec(main, spec) where

import Prelude hiding (log)
import ATerm
import Syntax
import Result

import qualified ConcreteSemantics as C

import Paths_system_s

import qualified Data.HashMap.Lazy as M
import qualified Data.Text.IO as T

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =

  describe "haskell arrows" $ beforeAll parseArrowCaseStudy $ do

    it "union should work" $ \module_ -> do
      let l1 = C.convertToList [1,2,3,4]
          l2 = C.convertToList [2,4]
          t = tup l1 l2
      C.eval (Call "union_0_0" [] []) (stratEnv module_) (t,M.empty)
        `shouldBe`
           Success (C.convertToList [1,3,2,4], M.empty)

    it "concat should work" $ \module_ ->
      let l = C.convertToList (fmap C.convertToList [[1,2,3],[4,5],[],[6]])
      in C.eval (Call "concat_0_0" [] []) (stratEnv module_) (l,M.empty)
        `shouldBe`
           Success (C.convertToList [1,2,3,4,5,6], M.empty)

    it "free-pat-vars should work" $ \module_ ->
      let var x = C.Cons "Var" [x]
          tuple x y = C.Cons "Tuple" [x,C.convertToList y]
          t = tuple (tuple (var "a") [var "b"])
                    [tuple (var "c") [var "a"]]
      in C.eval (Call "free_pat_vars_0_0" [] []) (stratEnv module_) (t,M.empty)
          `shouldBe`
             Success (C.convertToList [var "b", var "c", var "a"], M.empty)

  where
    parseArrowCaseStudy = do
      file <- T.readFile =<< getDataFileName "case-studies/arrows/desugar2core.aterm"
      case parseModule =<< parseATerm file of
        Left e -> fail (show e)
        Right module_ -> return module_

    tup x y = C.Cons "" [x,y]
