{-# LANGUAGE OverloadedStrings #-}
module CaseStudiesSpec(main, spec) where

import           Prelude hiding (log)

import qualified ConcreteSemantics as C
import           SharedSemantics
import           Syntax

import           Paths_system_s

import           Data.ATerm
import qualified Data.HashMap.Lazy as M
import           Data.Result
import qualified Data.Text.IO as T

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =

  describe "haskell arrows" $ beforeAll parseArrowCaseStudy $ do

    it "union should work" $ \module_ -> do
      let l1 = convertToList [1,2,3,4]
          l2 = convertToList [2,4]
          t = tup l1 l2
      C.eval (stratEnv module_) (Call "union_0_0" [] []) (t,M.empty)
        `shouldBe`
           Success (convertToList [1,3,2,4], M.empty)

    it "concat should work" $ \module_ ->
      let l = convertToList (fmap convertToList [[1,2,3],[4,5],[],[6]])
      in C.eval (stratEnv module_) (Call "concat_0_0" [] []) (l,M.empty)
        `shouldBe`
           Success (convertToList [1,2,3,4,5,6], M.empty)

    it "free-pat-vars should work" $ \module_ ->
      let var x = C.Cons "Var" [x]
          tuple x y = C.Cons "Tuple" [x,convertToList y]
          t = tuple (tuple (var "a") [var "b"])
                    [tuple (var "c") [var "a"]]
      in C.eval (stratEnv module_) (Call "free_pat_vars_0_0" [] []) (t,M.empty)
          `shouldBe`
             Success (convertToList [var "b", var "c", var "a"], M.empty)

  where
    parseArrowCaseStudy = do
      file <- T.readFile =<< getDataFileName "case-studies/arrows/arrows.aterm"
      case parseModule =<< parseATerm file of
        Left e -> fail (show e)
        Right module_ -> return module_

    tup x y = C.Cons "" [x,y]

