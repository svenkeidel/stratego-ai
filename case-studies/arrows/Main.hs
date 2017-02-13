{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (log)
import ATerm
import Syntax
import Result

import HaskellPretty
import qualified ConcreteSemantics as C
import qualified WildcardSemantics as W

import Paths_system_s

import qualified Data.HashMap.Lazy as M
import qualified Data.Text.IO as T
import qualified Data.Sequence as S

main :: IO ()
main = do
  file <- T.readFile =<< getDataFileName "case-studies/arrows/desugar2core.aterm"
  case parseModule =<< parseATerm file of
    Left e -> fail (show e)
    Right module_ -> do

      putStrLn "W.eval 5 (call union) Wildcard"
      print $ ppResults
            $ W.eval 5 (Call "union_0_0" [] [])
                (stratEnv module_) (W.Wildcard,M.empty)

      putStrLn "W.eval 1 (call desugar_arrow) Wildcard"
      print $ ppResults
            $ W.eval 1 (Call "desugar_arrow_0_0" [] [])
                (stratEnv module_) (W.Wildcard,M.empty)
      putStrLn ""

      putStrLn "W.eval 2 (call desugar_arrow) Wildcard"
      print $ ppResults
            $ W.eval 2 (Call "desugar_arrow_0_0" [] [])
                (stratEnv module_) (W.Wildcard,M.empty)
      putStrLn ""

      putStrLn "W.eval 3 (call desugar_arrow) Wildcard"
      print $ ppResults
            $ W.eval 3 (Call "desugar_arrow_0_0" [] [])
                (stratEnv module_) (W.Wildcard,M.empty)
      putStrLn ""

      putStrLn "W.eval 4 (call desugar_arrow) Wildcard"
      print $ S.length
            $ W.eval 4 (Call "desugar_arrow_0_0" [] [])
                (stratEnv module_) (W.Wildcard,M.empty)
      return ()
