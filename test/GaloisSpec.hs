{-# LANGUAGE OverloadedStrings #-}
module GaloisSpec (main, spec) where
    
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (Result(..))
import qualified ConcreteSemantics as C
import qualified WildcardSemantics as W
import           Data.Order
import           Data.Result
import qualified Data.Powerset as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "alpha_pow_result" $ do
    it "should produce something that is not wrong" $ do
      alpha (P.fromFoldable [Success (C.Cons "f" [])])
        `shouldBe`
          P.fromFoldable [Success (W.Cons "f" [])]
