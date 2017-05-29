{-# LANGUAGE OverloadedStrings #-}
module SubtypingSpec(main, spec) where

import           SubtypeRelation as S
import           Sort

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let rel = S.insert "B" "A"
          $ S.insert "C" "B"
          $ S.insert "B'" "A"
          $ S.insert "C" "B'"
            S.empty

  it "is a reflexive relation" $ do
    S.subtype rel "A" "A" `shouldBe` True
    S.subtype rel "B" "B" `shouldBe` True
    S.subtype rel "B'" "B'" `shouldBe` True
    S.subtype rel "C" "C" `shouldBe` True

  it "is a transitive relation" $ do
    S.subtype rel "C" "B" `shouldBe` True
    S.subtype rel "C" "B'" `shouldBe` True
    S.subtype rel "B" "A" `shouldBe` True
    S.subtype rel "B'" "A" `shouldBe` True

    S.subtype rel "C" "A" `shouldBe` True

  it "handles list" $ do
    S.subtype rel (List "C") (List "A") `shouldBe` True
    S.subtype rel (List "C") (List "B") `shouldBe` True
    S.subtype rel (List "C") (List "C") `shouldBe` True

  it "handles option" $ do
    S.subtype rel (Option "C") (Option "A") `shouldBe` True
    S.subtype rel (Option "C") (Option "B") `shouldBe` True
    S.subtype rel (Option "C") (Option "C") `shouldBe` True

  it "handles Tuple" $ do
    S.subtype rel (Tuple ["C","C"]) (Tuple ["A","A"]) `shouldBe` True
    S.subtype rel (Tuple ["B","C"]) (Tuple ["A","B"]) `shouldBe` True
    S.subtype rel (Tuple ["B","C"]) (Tuple ["B","C"]) `shouldBe` True
    S.subtype rel (Tuple ["B","B"]) (Tuple ["B","C"]) `shouldBe` False
    S.subtype rel (Tuple ["B","B"]) (Tuple ["B","B","B"]) `shouldBe` False

  it "contains a smallest element" $ do
    and [S.subtype rel Bottom s | s <- ["A","B","B'","C"]]
      `shouldBe` True
    or [S.subtype rel s Bottom | s <- ["A","B","B'","C"]]
      `shouldBe` False

  describe "least upper bounds" $ do
    let rel' = S.insert "C'" "B"
             $ S.insert "C'" "B'"
               rel

    it "are not necessarily unique" $ do
      S.lubs rel' ["C","C'"] `shouldMatchList` ["B'", "B"]
      S.lubs rel' [Tuple ["C", "C'"], Tuple ["C'","C"]] `shouldMatchList`
        [ Tuple ["B'", "B'"],
          Tuple ["B", "B'"],
          Tuple ["B'", "B"],
          Tuple ["B", "B"]
        ]

  let leastUpperBounds sort f = 
        describe ("least upper bounds of " ++ sort) $ do
          it "is reflexive" $ do
            S.lubs rel [f "C",f "C"] `shouldBe` [f "C"]
            S.lubs rel [f "B", f "B"] `shouldBe` [f "B"]
            S.lubs rel [f "B'", f "B'"] `shouldBe` [f "B'"]
            S.lubs rel [f "A", f "A"] `shouldBe` [f "A"]
          
          it "respects the subtyping order" $ do
            S.lubs rel [f "C", f "B"] `shouldBe` [f "B"]
            S.lubs rel [f "C", f "B'"] `shouldBe` [f "B'"]
            S.lubs rel [f "C", f "A"] `shouldBe` [f "A"]
          
          it "is commutative" $ do
            S.lubs rel [f "C", f "B"] `shouldBe` [f "B"]
            S.lubs rel [f "B", f "C"] `shouldBe` [f "B"]
            S.lubs rel [f "C", f "B'"] `shouldBe` [f "B'"]
            S.lubs rel [f "B'", f "C"] `shouldBe` [f "B'"]
            S.lubs rel [f "C", f "A"] `shouldBe` [f "A"]
            S.lubs rel [f "A", f "C"] `shouldBe` [f "A"]
      
          it "finds non-trivial upper bounds" $
            S.lubs rel [f "B", f "B'"] `shouldBe` [f "A"]

  leastUpperBounds "regular types" id
  leastUpperBounds "list types" List
  leastUpperBounds "option types" Option

  describe "least upper bounds of tuples" $ do
    it "is reflexive" $
      S.lubs rel [Tuple ["C", "B"], Tuple ["C", "B"]] `shouldBe` [Tuple ["C", "B"]]
    
    it "respects the subtyping order" $
      S.lubs rel [Tuple ["C", "C"], Tuple ["B", "B'"]] `shouldBe` [Tuple ["B", "B'"]]
    
    it "is commutative" $
      S.lubs rel [Tuple ["B", "B'"], Tuple ["C", "C"]] `shouldBe` [Tuple ["B", "B'"]]

    it "finds non-trivial upper bounds" $
      S.lubs rel [Tuple ["B"], Tuple ["B'"]] `shouldBe` [Tuple ["A"]]
