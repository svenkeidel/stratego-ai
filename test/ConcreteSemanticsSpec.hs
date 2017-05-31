{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ConcreteSemanticsSpec(main, spec) where

import           Prelude hiding ((.),id,succ,pred,all,fail,sequence,map,(<=))
import qualified Prelude as P

import           ConcreteSemantics
import           SharedSemantics
import           Syntax hiding (Fail,TermPattern(..))
import qualified Syntax as T

import           Paths_system_s

import           Control.Arrow
import           Control.Arrow.Try

import           Data.ATerm
import           Data.Result
import qualified Data.HashMap.Lazy as M
    
import           Test.Hspec
import           Test.Hspec.QuickCheck

import qualified Data.Text.IO as TIO

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  
  describe "guarded choice" $ do
    it "should execute the second strategy if the first succeeds" $
      runKleisli (guardedChoice succ succ pred) n `shouldBe` Just (n + 2)
    it "should execute the third strategy if the first fails" $
      runKleisli (guardedChoice fail succ pred) n `shouldBe` Just (n - 1)

  describe "sequence" $ do
    it "should thread the results of applying the first strategy to the second" $
      runKleisli (sequence succ succ) n `shouldBe` Just (n + 2)
    it "should fail if at least one of the specified strategy fails" $ do
      runKleisli (sequence fail succ) n `shouldBe` (Nothing :: Maybe Int)
      runKleisli (sequence succ fail) n `shouldBe` (Nothing :: Maybe Int)

  describe "one" $ do
    it "should apply an strategy to one subterm non-deterministically" $ do
      runKleisli (one succ) [1,2,3,4,5,6] `shouldBe`
        [ [2,2,3,4,5,6]
        , [1,3,3,4,5,6]
        , [1,2,4,4,5,6]
        , [1,2,3,5,5,6]
        , [1,2,3,4,6,6]
        , [1,2,3,4,5,7]
        ]
      runKleisli (one incEven) [1,2,3,4,5,6] `shouldBe`
        [ [1,2,3,4,5,6]
        , [1,3,3,4,5,6]
        , [1,2,3,4,5,6]
        , [1,2,3,5,5,6]
        , [1,2,3,4,5,6]
        , [1,2,3,4,5,7]
        ]
    it "should fail if the strategy fails on all subterms" $
      runKleisli (one fail) [1,2,3,4,5,6::Int] `shouldBe` (runKleisli fail () :: [[Int]])

  describe "some" $ do
    it "should apply an arrow to as many subterms as possible" $ do
      runKleisli (some succ) [1,2,3,4,5,6] `shouldBe` Just [2,3,4,5,6,7]
      runKleisli (some incEven) [1,2,3,4,5,6] `shouldBe` Just [1,3,3,5,5,7]
    it "should fail if the specified strategy fails for all suterms" $
      runKleisli (some fail) [1,2,3,4,5,6] `shouldBe` (Nothing :: Maybe [Int])

  describe "all" $ do
    it "should apply an arrow all subterms" $ do
      runKleisli (all succ) [1,2,3,4,5,6] `shouldBe` Just [2,3,4,5,6,7]
      runKleisli (all fail) ([]::[Int]) `shouldBe` (Just [] :: Maybe [Int])
    it "should fail if the specified strategy fails for at least one subterm" $
      runKleisli (all fail) ([1,2,3,4,5,6]::[Int]) `shouldBe` (Nothing :: Maybe [Int])

  describe "scope" $ do
    it "should hide declare variables" $ do
      let tenv = M.fromList [("x", term1)]
      eval M.empty (Scope ["x"] (Build "x")) (term2,tenv)
        `shouldBe` Fail
      eval M.empty (Scope ["x"] (Match "x")) (term2,tenv)
        `shouldBe` Success (term2,tenv)

    it "should make non-declared variables available" $ do
      let tenv = M.fromList [("x", term1)]
      eval M.empty (Scope ["y"] (Build "x")) (term2,tenv) `shouldBe`
        Success (term1,tenv)
      eval M.empty (Scope ["y"] (Match "z")) (term2,tenv) `shouldBe`
        Success (term2,M.fromList [("x", term1), ("z", term2)])

  describe "let" $
    it "should support recursion" $ do
      let t = convertToList (NumberLiteral <$> [2, 3, 4])
      eval M.empty (Let [("map", map)] (Match "x" `Seq` Call "map" [Build 1] ["x"])) (t,M.empty)
        `shouldBe`
           Success (convertToList [1, 1, 1], M.fromList [("x",t)])

  describe "call" $
    it "should support recursion" $ do
      let senv = M.fromList [("map", Closure map M.empty)]
      let t = convertToList [2, 3, 4]
      eval senv (Match "x" `Seq` Call "map" [Build (T.NumberLiteral 1)] ["x"]) (t, M.empty)
        `shouldBe`
           Success (convertToList [1, 1, 1], M.fromList [("x",t)])

  describe "match" $ do
    prop "should introduce variables" $ \t ->
      eval M.empty (Match "x" `Seq` Match "y") (t,M.empty) `shouldBe`
        Success (t, M.fromList [("x", t), ("y", t)])

    prop "should support linear pattern matching" $ \t1 t2 ->
      let t' = Cons "f" [t1,t2]
      in eval M.empty (Match (T.Cons "f" ["x","x"])) (t',M.empty) `shouldBe`
           if t1 == t2 then Success (t', M.fromList [("x", t1)]) else Fail

    it "should succeed when exploding literals" $
      eval M.empty (Match (T.Explode "_" "x")) (1, M.empty) `shouldBe`
        Success (1, M.fromList [("x", Cons "Nil" [])])

  describe "Case Studies" $ describe "Haskell Arrows" $ beforeAll parseArrowCaseStudy $ do

    it "union should work" $ \module_ -> do
      let l1 = convertToList [1,2,3,4]
          l2 = convertToList [2,4]
          t = tup l1 l2
      eval (stratEnv module_) (Call "union_0_0" [] []) (t,M.empty)
        `shouldBe`
           Success (convertToList [1,3,2,4], M.empty)

    it "concat should work" $ \module_ ->
      let l = convertToList (fmap convertToList [[1,2,3],[4,5],[],[6]])
      in eval (stratEnv module_) (Call "concat_0_0" [] []) (l,M.empty)
        `shouldBe`
           Success (convertToList [1,2,3,4,5,6], M.empty)

    it "free-pat-vars should work" $ \module_ ->
      let var x = Cons "Var" [x]
          tuple x y = Cons "Tuple" [x,convertToList y]
          t = tuple (tuple (var "a") [var "b"])
                    [tuple (var "c") [var "a"]]
      in eval (stratEnv module_) (Call "free_pat_vars_0_0" [] []) (t,M.empty)
          `shouldBe`
             Success (convertToList [var "b", var "c", var "a"], M.empty)

  where

    map = Strategy ["f"] ["l"] (Scope ["x","xs","x'","xs'"] (
            Build "l" `Seq`
            GuardedChoice
              (Match (T.Cons "Cons" ["x","xs"]))
              (Build "x" `Seq`
               Call "f" [] [] `Seq`
               Match "x'" `Seq`
               Call "map" ["f"] ["xs"] `Seq`
               Match "xs'" `Seq`
               Build (T.Cons "Cons" ["x'", "xs'"]))
              (Build (T.Cons "Nil" []))))

    
    term1 = NumberLiteral 1
    term2 = NumberLiteral 2

    succ :: Arrow p => p Int Int
    succ = arr (+ 1)

    pred :: Arrow p => p Int Int
    pred = arr (\i -> i - 1)

    incEven :: (Arrow p) => p Int Int
    incEven = arr (\i -> if even (i::Int) then i+1 else i)

    n :: Int
    n = 1

    parseArrowCaseStudy = do
      file <- TIO.readFile =<< getDataFileName "case-studies/arrows/arrows.aterm"
      case parseModule =<< parseATerm file of
        Left e -> P.fail (show e)
        Right module_ -> return module_

    tup x y = Cons "" [x,y]


instance Monoid Int where
  mempty = undefined
  mappend = undefined
