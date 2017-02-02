{-# LANGUAGE OverloadedStrings #-}
module InterpreterSpec(main, spec) where

import Prelude hiding ((.),id,succ,pred,all,fail,sequence)

import Syntax
import Interpreter

import Control.Category
import Control.Arrow

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "the strategy guarded choice" $ do
    it "should execute the second strategy if the first succeeds" $
      runKleisli (guardedChoice succ succ pred) n `shouldBe` Just (n + 2)
    it "should execute the third strategy if the first fails" $
      runKleisli (guardedChoice fail succ pred) n `shouldBe` Just (n - 1)

  describe "the strategy sequence" $ do
    it "should thread the results of applying the first strategy to the second" $
      runKleisli (sequence succ succ) n `shouldBe` Just (n + 2)
    it "should fail if at least one of the specified strategy fails" $ do
      runKleisli (sequence fail succ) n `shouldBe` (Nothing :: Maybe Int)
      runKleisli (sequence succ fail) n `shouldBe` (Nothing :: Maybe Int)

  describe "the strategy congruence" $ do
    it "should fail if the constructors are not equal" $
      runKleisli (cong "g" (replicate 5 succ)) ("f",[1,2,3,4,5,6::Int]) `shouldBe` (Nothing :: Maybe (Constructor,[Int]))
    it "should fail if the arities of the terms does not match" $
      runKleisli (cong "f" (replicate 5 succ)) ("f",[1,2,3,4,5,6]) `shouldBe` (Nothing :: Maybe (Constructor,[Int]))
    it "should in all other cases apply arrows element wise" $
      runKleisli (cong "f" [id,succ,id]) ("f",[1,2,3]) `shouldBe` (Just ("f",[1,3,3]) :: Maybe (Constructor,[Int]))

  describe "the strategy one" $ do
    it "should apply an strategy to one subterm non-deterministically" $ do
      runKleisli (one succ) ("f",[1,2,3,4,5,6]) `shouldBe`
        [ ("f",[2,2,3,4,5,6])
        , ("f",[1,3,3,4,5,6])
        , ("f",[1,2,4,4,5,6])
        , ("f",[1,2,3,5,5,6])
        , ("f",[1,2,3,4,6,6])
        , ("f",[1,2,3,4,5,7])
        ]
      runKleisli (one incEven) ("f",[1,2,3,4,5,6]) `shouldBe`
        [ ("f",[1,2,3,4,5,6])
        , ("f",[1,3,3,4,5,6])
        , ("f",[1,2,3,4,5,6])
        , ("f",[1,2,3,5,5,6])
        , ("f",[1,2,3,4,5,6])
        , ("f",[1,2,3,4,5,7])
        ]
    it "should fail if the strategy fails on all subterms" $
      runKleisli (one fail) ("f",[1,2,3,4,5,6::Int]) `shouldBe` (runKleisli fail () :: [(Constructor,[Int])])

  describe "the strategy some" $ do
    it "should apply an arrow to as many subterms as possible" $ do
      runKleisli (some succ) ("f",[1,2,3,4,5,6]) `shouldBe` Just ("f",[2,3,4,5,6,7])
      runKleisli (some incEven) ("f",[1,2,3,4,5,6]) `shouldBe` Just ("f",[1,3,3,5,5,7])
    it "should fail if the specified strategy fails for all suterms" $
      runKleisli (some fail) ("f",[1,2,3,4,5,6]) `shouldBe` (Nothing :: Maybe (Constructor,[Int]))

  describe "the strategy all" $ do
    it "should apply an arrow all subterms" $ do
      runKleisli (all succ) ("f",[1,2,3,4,5,6]) `shouldBe` Just ("f",[2,3,4,5,6,7])
      runKleisli (all fail) ("f",[]::[Int]) `shouldBe` (Just ("f",[]) :: Maybe (Constructor,[Int]))
    it "should fail if the specified strategy fails for at least one subterm" $
      runKleisli (all fail) ("f",[1,2,3,4,5,6]::[Int]) `shouldBe` (Nothing :: Maybe (Constructor,[Int]))

  describe "nth" $ do
    it "should apply an arrow to the nth element of a list" $ do
      runKleisli (nth succ) (1,[1,2,3,4,5,6]) `shouldBe` Just [2,2,3,4,5,6]
      runKleisli (nth succ) (2,[1,2,3,4,5,6]) `shouldBe` Just [1,3,3,4,5,6]
      runKleisli (nth succ) (3,[1,2,3,4,5,6]) `shouldBe` Just [1,2,4,4,5,6]
      runKleisli (nth succ) (4,[1,2,3,4,5,6]) `shouldBe` Just [1,2,3,5,5,6]
      runKleisli (nth succ) (5,[1,2,3,4,5,6]) `shouldBe` Just [1,2,3,4,6,6]
      runKleisli (nth succ) (6,[1,2,3,4,5,6]) `shouldBe` Just [1,2,3,4,5,7]
    it "should fail if the index is out of bounds" $
      runKleisli (nth succ) (7,[1,2,3,4,5,6]) `shouldBe` Nothing

  where
    succ :: Arrow p => p Int Int
    succ = arr (+ 1)

    pred :: Arrow p => p Int Int
    pred = arr (\i -> i - 1)

    incEven :: (Arrow p) => p Int Int
    incEven = arr (\i -> if even (i::Int) then i+1 else i)

    n :: Int
    n = 1
