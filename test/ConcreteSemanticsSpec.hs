{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Control.Monad

import           Data.ATerm
import           Data.Result
import qualified Data.HashMap.Lazy as M
import           Data.TermEnv
    
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (Result(..))

import qualified Data.Text.IO as TIO

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  
  describe "scope" $ do
    it "should hide declare variables" $ do
      let tenv = concreteTermEnv [("x", term1)]
      eval M.empty (Scope ["x"] (Build "x")) (term2,tenv)
        `shouldBe` Fail
      eval M.empty (Scope ["x"] (Match "x")) (term2,tenv)
        `shouldBe` Success (term2,tenv)

    it "should make non-declared variables available" $ do
      let tenv = concreteTermEnv [("x", term1)]
      eval M.empty (Scope ["y"] (Build "x")) (term2,tenv) `shouldBe`
        Success (term1,tenv)
      eval M.empty (Scope ["y"] (Match "z")) (term2,tenv) `shouldBe`
        Success (term2,concreteTermEnv [("x", term1), ("z", term2)])

  describe "let" $
    it "should support recursion" $ do
      let t = convertToList (NumberLiteral <$> [2, 3, 4])
          tenv = concreteTermEnv []; tenv' = concreteTermEnv [("x",t)]
      eval M.empty (Let [("map", map)] (Match "x" `Seq` Call "map" [Build 1] ["x"])) (t,tenv)
        `shouldBe`
           Success (convertToList [1, 1, 1], tenv')

  describe "call" $
    it "should support recursion" $ do
      let senv = M.fromList [("map", Closure map M.empty)]
          t = convertToList [2, 3, 4]
          tenv = concreteTermEnv []; tenv' = concreteTermEnv [("x",t)]
      eval senv (Match "x" `Seq` Call "map" [Build (T.NumberLiteral 1)] ["x"]) (t, tenv)
        `shouldBe`
           Success (convertToList [1, 1, 1], tenv')

  describe "match" $ do
    prop "should introduce variables" $ \t ->
      let tenv = concreteTermEnv [] 
      in eval M.empty (Match "x" `Seq` Match "y") (t,tenv) `shouldBe`
           Success (t, concreteTermEnv [("x", t), ("y", t)])

    prop "should support linear pattern matching" $ \t1 t2 ->
      let t' = Cons "f" [t1,t2]
          tenv = concreteTermEnv []; tenv' = concreteTermEnv [("x",t1)]
      in eval M.empty (Match (T.Cons "f" ["x","x"])) (t',tenv) `shouldBe`
           if t1 == t2 then Success (t', tenv') else Fail

    prop "should match deep" $ \t -> do
      p <- similarTermPattern t 3
      return $ counterexample (show p) $ when (linear p) $
        fmap fst (eval M.empty (Match p) (t,concreteTermEnv [])) `shouldBe`
          Success t

    it "should succeed when exploding literals" $
      let tenv = concreteTermEnv []; tenv' = concreteTermEnv [("x", Cons "Nil" [])]
      in eval M.empty (Match (T.Explode "_" "x")) (1, tenv) `shouldBe`
           Success (1, tenv')

  describe "build" $ do
    prop "build should be inverse to match" $ \t -> do
      p <- similarTermPattern t 3
      return $ counterexample (show p) $ when (linear p) $
        fmap fst (eval M.empty (Match p `Seq` Build p) (t,concreteTermEnv [])) `shouldBe`
          Success t

    prop "build should lookup variables" $ \t -> do
      let tenv = concreteTermEnv [("x", t)]
      eval M.empty (Build (T.Var "x")) (t,tenv) `shouldBe`
        Success (t,tenv)

  describe "Case Studies" $ describe "Haskell Arrows" $ beforeAll parseArrowCaseStudy $ do

    it "union should work" $ \module_ -> do
      let l1 = convertToList [1,2,3,4]
          l2 = convertToList [2,4]
          t = tup l1 l2
          tenv = concreteTermEnv []
      eval (stratEnv module_) (Call "union_0_0" [] []) (t,tenv)
        `shouldBe`
           Success (convertToList [1,3,2,4], tenv)

    it "concat should work" $ \module_ ->
      let l = convertToList (fmap convertToList [[1,2,3],[4,5],[],[6]])
          tenv = concreteTermEnv []
      in eval (stratEnv module_) (Call "concat_0_0" [] []) (l,tenv)
        `shouldBe`
           Success (convertToList [1,2,3,4,5,6], tenv)

    it "free-pat-vars should work" $ \module_ ->
      let var x = Cons "Var" [x]
          tuple x y = Cons "Tuple" [x,convertToList y]
          t = tuple (tuple (var "a") [var "b"])
                    [tuple (var "c") [var "a"]]
          tenv = ConcreteTermEnv M.empty
      in eval (stratEnv module_) (Call "free_pat_vars_0_0" [] []) (t,tenv)
          `shouldBe`
             Success (convertToList [var "b", var "c", var "a"], tenv)

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


