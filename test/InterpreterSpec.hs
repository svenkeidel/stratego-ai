{-# LANGUAGE OverloadedStrings #-}
module InterpreterSpec(main, spec) where

import           Prelude hiding ((.),id,succ,pred,all,fail,sequence,map)

import           Syntax hiding (Fail)
import           Interpreter
import           Result
import qualified ConcreteSemantics as C
import qualified WildcardSemantics as W
import           WildcardSemanticsSoundness

import           Control.Arrow

import           Data.Foldable (toList)
import           Data.Hashable
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as H
import           Data.Sequence (Seq)
import qualified Data.Sequence as S

import           Text.Printf

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (Result(..))

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

  describe "some" $ do
    it "should apply an arrow to as many subterms as possible" $ do
      runKleisli (some succ) ("f",[1,2,3,4,5,6]) `shouldBe` Just ("f",[2,3,4,5,6,7])
      runKleisli (some incEven) ("f",[1,2,3,4,5,6]) `shouldBe` Just ("f",[1,3,3,5,5,7])
    it "should fail if the specified strategy fails for all suterms" $
      runKleisli (some fail) ("f",[1,2,3,4,5,6]) `shouldBe` (Nothing :: Maybe (Constructor,[Int]))

  describe "all" $ do
    it "should apply an arrow all subterms" $ do
      runKleisli (all succ) ("f",[1,2,3,4,5,6]) `shouldBe` Just ("f",[2,3,4,5,6,7])
      runKleisli (all fail) ("f",[]::[Int]) `shouldBe` (Just ("f",[]) :: Maybe (Constructor,[Int]))
    it "should fail if the specified strategy fails for at least one subterm" $
      runKleisli (all fail) ("f",[1,2,3,4,5,6]::[Int]) `shouldBe` (Nothing :: Maybe (Constructor,[Int]))

  describe "scope" $ do
    it "should hide declare variables" $ do
      let tenv = M.fromList [("x", term1)]
      C.eval (Scope ["x"] (Build "x")) M.empty (term2,tenv)
        `shouldBe` Fail
      C.eval (Scope ["x"] (Match "x")) M.empty (term2,tenv)
        `shouldBe` Success (term2,tenv)

    it "should make non-declared variables available" $ do
      let tenv = M.fromList [("x", term1)]
      C.eval (Scope ["y"] (Build "x")) M.empty (term2,tenv) `shouldBe`
        Success (term1,tenv)
      C.eval (Scope ["y"] (Match "z")) M.empty (term2,tenv) `shouldBe`
        Success (term2,M.fromList [("x", term1), ("z", term2)])

  describe "let" $ do
    it "should support recursion" $ do
      let t = C.convertToList (C.NumberLiteral <$> [2, 3, 4])
      ceval (Let [("map", map)]
                  (Match "x" `Seq`
                   Call "map" [Build 1] ["x"])) t
        `shouldBe`
           Success (C.convertToList [1, 1, 1], M.fromList [("x",t)])

    it "should work for the abstract case" $ do
      let cons x xs = W.Cons "Cons" [x,xs]
      let t = cons 2 W.Wildcard
      fmap fst <$> weval 5 (Let [("map", map)]
                  (Match "x" `Seq`
                   Call "map" [Build 1] ["x"])) t
        `shouldBe'`
           S.fromList 
             [ Success $ W.convertToList [1]
             , Success $ W.convertToList [1,1]
             , Success $ W.convertToList [1,1,1]
             , Fail
             , Fail
             , Success (cons 1 (cons 1 (cons 1 (cons W.Wildcard W.Wildcard))))]

  describe "call" $ do
    it "should support recursion" $ do
      let senv = M.fromList [("map", Closure map M.empty)]
      let t = C.convertToList [2, 3, 4]
      C.eval (Match "x" `Seq`
              Call "map" [Build (NumberLiteral 1)] ["x"]) senv (t, M.empty)
        `shouldBe`
           Success (C.convertToList [1, 1, 1], M.fromList [("x",t)])

    prop "should be sound" $ do
      i <- choose (0,10)
      j <- choose (0,10)
      l <- C.similarTerms i 7 2 10
      let (l1,l2) = splitAt j l
      let t1 = C.convertToList l1
      let t2 = C.convertToList l2
      return $ counterexample (printf "t: %s\n"
                                      (show (lub (alphaTerm t1) (alphaTerm t2))))
             $ sound' 12 (Let [("map", map)]
                  (Match "x" `Seq`
                   Call "map" [Build 1] ["x"]))
                 (S.fromList [t1,t2])

  describe "match" $ do
    prop "should introduce variables" $ \t ->
      ceval (Match "x" `Seq` Match "y") t `shouldBe`
        Success (t, M.fromList [("x", t), ("y", t)])

    prop "should support linear pattern matching" $ \t1 t2 ->
      let t' = C.Cons "f" [t1,t2]
      in ceval (Match (Cons "f" ["x","x"])) t' `shouldBe`
           if t1 == t2 then Success (t', M.fromList [("x", t1)]) else Fail

    prop "should be sound" $ do
      i <- choose (0,1)
      [t1,t2,t3] <- C.similarTerms 3 7 2 10
      matchPattern <- C.similarTermPattern t1 3
      return $ counterexample
                 (printf "i: %d\npattern: %s\nt2: %s\nt3: %s\nlub t2 t3 = %s"
                    i (show matchPattern) (show t2) (show t3)
                    (show (lub (alphaTerm t2) (alphaTerm t3))))
             $ sound' i (Match matchPattern) (S.fromList [t2,t3])

    it "should succeed when exploding literals" $
      ceval (Match (Explode "_" "x")) 1 `shouldBe`
         Success (1, M.fromList [("x", C.Cons "Nil" [])])

  describe "build" $
    prop "should be sound" $ do
      i <- choose (0,1)
      [t1,t2,t3] <- C.similarTerms 3 7 2 10
      matchPattern <- C.similarTermPattern t1 3
      let vars = patternVars' matchPattern
      buildPattern <- arbitraryTermPattern 5 2 $
        if not (null vars) then elements vars else arbitrary
      return $ counterexample
                 (printf "match pattern: %s\nbuild pattern: %s\nt2: %s\nt3: %s\nlub t2 t3 = %s"
                    (show matchPattern) (show buildPattern) (show t2) (show t3)
                    (show (lub (alphaTerm t2) (alphaTerm t3))))
             $ sound' i (Match matchPattern `Seq` Build buildPattern) (S.fromList [t2,t3])

  describe "unify" $
    prop "should compare terms" $ \t1 t2 ->
      runKleisli C.unify (t1,t2) `shouldBe`
        if t1 == t2 then Just t1 else Nothing

  where

    shouldBe' :: (Show a, Eq a, Hashable a) => Seq a -> Seq a -> Expectation
    shouldBe' s1 s2 = H.fromList (toList s1) `shouldBe` H.fromList (toList s2)
    infix 1 `shouldBe'`

    map = Strategy ["f"] ["l"] (Scope ["x","xs","x'","xs'"] (
            Build "l" `Seq`
            GuardedChoice
              (Match (Cons "Cons" ["x","xs"]))
              (Build "x" `Seq`
               Call "f" [] [] `Seq`
               Match "x'" `Seq`
               Call "map" ["f"] ["xs"] `Seq`
               Match "xs'" `Seq`
               Build (Cons "Cons" ["x'", "xs'"]))
              (Build (Cons "Nil" []))))

    term1 = C.NumberLiteral 1
    term2 = C.NumberLiteral 2

    ceval :: Strat -> C.Term -> Result (C.Term,C.TermEnv)
    ceval s t = C.eval s M.empty (t,M.empty)

    weval :: Int -> Strat -> W.Term -> Seq (Result (W.Term,W.TermEnv))
    weval i s t = W.eval i s M.empty (t,M.empty)

    succ :: Arrow p => p Int Int
    succ = arr (+ 1)

    pred :: Arrow p => p Int Int
    pred = arr (\i -> i - 1)

    incEven :: (Arrow p) => p Int Int
    incEven = arr (\i -> if even (i::Int) then i+1 else i)

    n :: Int
    n = 1
