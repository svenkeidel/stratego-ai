{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (log)
import ATerm
import Syntax hiding (Fail)

import PrettyPrint
import qualified HaskellPretty as H
import qualified PCFPretty as P
import qualified WildcardSemantics as W

import Paths_system_s

import Control.Monad

import Data.String
import qualified Data.HashMap.Lazy as M
import qualified Data.Text.IO as T

import Text.Printf

import Text.PrettyPrint hiding (sep)

main :: IO ()
main = do
  caseStudy "arrows" "desugar_arrow_0_0" 3 H.ppHaskell
  caseStudy "pcf" "eval_0_0" 4 P.ppPCF
  caseStudy "pcf" "check_eval_0_0" 4 P.ppPCF
  caseStudy "pcf" "check_0_0" 4 P.ppType
  caseStudy "pcf" "check_num_0_0" 4 P.ppType

type PPrint = W.Term -> Doc

caseStudy :: String -> String -> Int -> PPrint -> IO ()
caseStudy name function depth pprint = do
  file <- T.readFile =<< getDataFileName (printf "case-studies/%s/%s.aterm" name name)
  case parseModule =<< parseATerm file of
    Left e -> fail (show e)
    Right module_ -> forM_ [1..depth::Int] $ \n -> do
      printf "W.eval %d (call %s) Wildcard\n" n function
      let res = W.eval n (stratEnv module_) (Call (fromString function) [] []) (W.Wildcard,M.empty)
      print $ ppResults pprint res
      putStrLn ""
