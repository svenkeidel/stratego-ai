{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (log)
import ATerm
import Syntax hiding (Fail)
import Result

import PrettyPrint
import qualified HaskellPretty as H
import qualified PCFPretty as P
import qualified WildcardSemantics as W
import qualified WildcardSemanticsDelayed as W
import qualified WildcardSemanticsSoundness as W

import Paths_system_s

import Control.Monad
import Control.Arrow.Transformer.Deduplicate

import Data.String
import qualified Data.HashMap.Lazy as M
import qualified Data.Text.IO as T
import qualified Data.Sequence as S

import System.IO

import Text.Printf

import Text.PrettyPrint hiding (sep)

main :: IO ()
main =
  withFile "casestudy.csv" WriteMode $ \csv -> do
    hPrintf csv "name,fun,depth,height,size,wittness\n"
    caseStudy csv "arrows" "desugar_arrow_0_0" 4 H.ppHaskell
    caseStudy csv "pcf" "eval_0_0" 5 P.ppPCF
    caseStudy csv "pcf" "check_eval_0_0" 5 P.ppPCF
    caseStudy csv "pcf" "check_0_0" 5 P.ppType
    caseStudy csv "pcf" "check_num_0_0" 5 P.ppType
    caseStudy csv "cca" "norm_0_0" 5 H.ppHaskell

type PPrint = W.Term -> Doc

caseStudy :: Handle -> String -> String -> Int -> PPrint -> IO ()
caseStudy csv name function maxDepth pprint = do
  printf "------------------ case study: %s ----------------------\n" name
  file <- T.readFile =<< getDataFileName (printf "case-studies/%s/%s.aterm" name name)
  case parseModule =<< parseATerm file of
    Left e -> fail (show e)
    Right module_ -> forM_ [1..maxDepth::Int] $ \depth -> do
      printf "function: %s\tdepth: %d\n" function depth
      let res = W.eval depth (stratEnv module_) (Call (fromString function) [] []) (W.Wildcard,M.empty)
          res' = dedup' $ filterResults res
      if S.length res' <= 200
         then print $ ppResults pprint res
         else printf "Output ommited because of result set contains %d element\n" (S.length res')
      forM_ res $ \r -> case r of
        Success (t,_) -> hPrintf csv "%s,%s,%d,%d,%d,%s\n" name function depth (W.height t) (W.size t) (show (W.isWittness t res'))
        _ -> return ()
      putStrLn ""
 where
   filterResults = fmap (\r -> case r of Success (t,_) -> t; Fail -> error "")
                . S.filter (\r -> case r of Success _ -> True; _ -> False)
