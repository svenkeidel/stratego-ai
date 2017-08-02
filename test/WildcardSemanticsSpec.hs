{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module WildcardSemanticsSpec(main, spec) where

import           Prelude hiding ((.),id,succ,pred,all,fail,sequence,map,(<=))

import qualified ConcreteSemantics as C
import           SharedSemantics
import           Syntax hiding (Fail)
import qualified WildcardSemantics as W
import qualified WildcardSemanticsDelayed as W
import           Soundness
import           InterpreterArrow


import           Control.Arrow
import qualified Data.HashMap.Lazy as M
import           Data.Hashable
import           Data.Result
import           Data.Order
import           Data.TermEnv
import           Data.Term(TermUtils(..))
import           Data.Powerset (Pow,fromFoldable)
import qualified Data.Powerset as P
import           Data.PowersetResult (PowersetResult)
    
import           Text.Printf

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (Result(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "let" $
    it "should work for the abstract case" $ do
      let cons x xs = W.Cons "Cons" [x,xs]
      let t = cons 2 W.Wildcard
      fmap fst <$> weval 2 (Let [("map", map)]
                  (Match "x" `Seq`
                   Call "map" [Build 1] ["x"])) t
        `shouldBe'`
           fromFoldable
             [ Success $ convertToList [1]
             , Success $ convertToList [1,1]
             , Success $ convertToList [1,1,1]
             , Fail
             , Fail
             , Success (cons 1 (cons 1 (cons 1 (cons W.Wildcard W.Wildcard))))]

  describe "call" $
    prop "should be sound" $ do
      i <- choose (0,10)
      j <- choose (0,10)
      l <- C.similarTerms i 7 2 10
      let (l1,l2) = splitAt j l
      let t1 = convertToList l1
      let t2 = convertToList l2
      return $ counterexample (printf "t: %s\n"
                                      (showLub t1 t2))
             $ sound' 2 (Let [("map", map)]
                  (Match "x" `Seq`
                   Call "map" [Build 1] ["x"]))
                  [(t1,[]),(t2,[])]

  describe "match" $ do

    prop "should handle inconsistent environments" $ do
      let t1 = C.Cons "f" []
          t2 = C.Cons "g" []
      sound' 1 (Match "x") [(t1, [("x", t1)]), (t2, [("y", t2)])]

    prop "should be sound" $ do
      i <- choose (0,1)
      [t1,t2,t3] <- C.similarTerms 3 7 2 10
      matchPattern <- C.similarTermPattern t1 3
      return $ counterexample
                 (printf "i: %d\npattern: %s\nt2: %s\nt3: %s\nlub t2 t3 = %s"
                    i (show matchPattern) (show t2) (show t3)
                    (showLub t2 t3))
             $ sound' i (Match matchPattern) [(t2,[]),(t3,[])]

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
                    (showLub t2 t3))
             $ sound' i (Match matchPattern `Seq` Build buildPattern) [(t2,[]),(t3,[])]

  -- describe "lookupTermVar" $
  --   prop "should be sound" $
  --     sound'' lookupTermVar lookupTermVar 

  where
    
    sound' :: Int -> Strat -> [(C.Term,[(TermVar,C.Term)])] -> Property
    sound' i s xs = sound'' (C.eval'' s) (W.eval'' i s) (fmap (\(t,tenv) -> (t,concreteTermEnv tenv)) xs)

    sound'' :: (Eq a, Eq b, Hashable a, Hashable b, Galois (Pow a) a' (->), Galois (Pow b) b' (->), Show b, Show b')
            => Interp StratEnv (ConcreteTermEnv C.Term) Result a b
            -> Interp StratEnv (AbstractTermEnv W.Term) PowersetResult a' b'
            -> [(a,ConcreteTermEnv C.Term)]
            -> Property
    sound'' f g xs = sound M.empty f g (fromFoldable xs)

    showLub :: C.Term -> C.Term -> String
    showLub = curry (show <<< (alpha :: Pow C.Term -> W.Term) <<< arr (\(t1,t2) -> P.fromFoldable [t1,t2]))

    shouldBe' :: Pow (Result W.Term) -> Pow (Result W.Term) -> Property
    shouldBe' s1 s2 = counterexample (printf "%s < %s\n" (show s1) (show s2)) (((⊑) (s2,s1)) `shouldBe` True)
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

    weval :: Int -> Strat -> W.Term -> Pow (Result (W.Term,W.TermEnv))
    weval i s t = W.eval i M.empty s (t,AbstractTermEnv M.empty)
