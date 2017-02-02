{-# LANGUAGE OverloadedStrings #-}
module SyntaxSpec(main, spec) where

import ATerm
import Syntax

import qualified Data.Text.IO as T

import Paths_system_s

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =

  describe "the syntax" $
    it "can be constructed from an aterm " $ do
      file <- T.readFile =<< getDataFileName "case-studies/arrows/desugar2core.aterm"
      let modul = parseModule =<< parseATerm file
      shouldNotProduceAnError modul

  where
    shouldNotProduceAnError :: Show a => Either a b -> Expectation
    shouldNotProduceAnError (Right _) = return ()
    shouldNotProduceAnError (Left a) = expectationFailure (show a)
