{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (log)
import ATerm
import Syntax hiding (Fail)
import Result

import PrettyPrint
import RegularTreeGrammar
import qualified HaskellPretty as H
import qualified PCFPretty as P
import qualified WildcardSemantics as W
import qualified WildcardSemanticsDelayed as W
import qualified WildcardSemanticsSoundness as W

import Paths_system_s

import qualified Criterion.Types as CT
import qualified Criterion.Measurement as CM
import Control.Monad
import Control.Arrow.Transformer.Deduplicate

import Data.Monoid
import Data.String
import Data.HashSet (HashSet)
import Data.Foldable
import qualified Data.HashSet as H
import qualified Data.HashMap.Lazy as M
import qualified Data.Text.IO as T
import Data.Sequence (Seq)
import qualified Data.Sequence as S

import System.IO

import Text.Printf

import Text.PrettyPrint hiding (sep,(<>))

main :: IO ()
main =
  sizeAnalysisSetup $ \sizeAnalysis ->
  heightAnalysisSetup $ \heightAnalysis ->
  wittnessAnalysisSetup $ \wittnessAnalysis ->
  ruleInvocationsAnalysisSetup $ \ruleInvocationsAnalysis ->
  classificationSetup $ \classification -> do
    CM.initializeTime

    deactivate $ caseStudy "arrows" "desugar_arrow_0_0" 4 $
      prettyPrint H.ppHaskell <>
      sizeAnalysis <>
      heightAnalysis <>
      wittnessAnalysis

    deactivate $ caseStudy "cca" "norm_0_0" 5 $
      prettyPrint H.ppHaskell <>
      sizeAnalysis <>
      heightAnalysis <>
      wittnessAnalysis

    activate $ caseStudy "pcf" "eval_0_0" 4 $
      -- prettyPrint P.ppPCF <>
      sizeAnalysis <>
      heightAnalysis <>
      wittnessAnalysis <>
      ruleInvocationsAnalysis pcfEvalGrammar <>
      classification 4 pcfEvalGrammar

    activate $ caseStudy "pcf" "check_eval_0_0" 5 $
      -- prettyPrint P.ppPCF <>
      sizeAnalysis <>
      heightAnalysis <>
      wittnessAnalysis <>
      ruleInvocationsAnalysis pcfCheckEvalGrammar <>
      classification 4 pcfCheckEvalGrammar
                    
  where
    activate :: IO () -> IO ()
    activate cs = cs

    deactivate :: IO () -> IO ()
    deactivate _ = return ()

prettyPrint :: (W.Term -> Doc) -> Analysis
prettyPrint pprint _ _ _ res =
  if H.size res <= 200
     then print $ ppResults pprint (toList res)
     else printf "Output ommited because of result set contains %d element\n" (H.size res)

sizeAnalysisSetup :: (Analysis -> IO ()) -> IO ()
sizeAnalysisSetup k =
  withFile "size.csv" WriteMode $ \csv -> do
    hPrintf csv "name;fun;depth;term;size\n"
    k $ \name fun depth res -> measure "Size Analysis" $ forM_ res $ \t ->
      hPrintf csv "%s;%s;%d;%s;%d\n" name fun depth (show t) (W.size t)

heightAnalysisSetup :: (Analysis -> IO ()) -> IO ()
heightAnalysisSetup k =
  withFile "height.csv" WriteMode $ \csv -> do
    hPrintf csv "name;fun;depth;term;height\n"
    k $ \name fun depth res -> measure "Height Analysis" $ forM_ res $ \t ->
        hPrintf csv "%s;%s;%d;%s;%d\n" name fun depth (show t) (W.height t)

wittnessAnalysisSetup :: (Analysis -> IO ()) -> IO ()
wittnessAnalysisSetup k =
  withFile "wittness.csv" WriteMode $ \csv -> do
    hPrintf csv "name;fun;depth;term;wittness\n"
    k $ \name fun depth res -> measure "Wittness Analysis" $ forM_ res $ \t ->
        hPrintf csv "%s;%s;%d;%s;%s\n" name fun depth (show t) (show (W.isWittness t res))

ruleInvocationsAnalysisSetup :: ((Grammar -> Analysis) -> IO ()) -> IO ()
ruleInvocationsAnalysisSetup k =
  withFile "rule.csv" WriteMode $ \csv -> do
    hPrintf csv "name;fun;depth;term;ruleId;rule;invocation\n"
    k $ \grammar name fun depth res -> measure "Rule Invocations Analysis" $ forM_ res $ \t ->
      case summary grammar t of
        Just s -> forM_ (M.toList s `zip` [(1::Int)..]) $ \((rule,count),ruleId) ->
          hPrintf csv "%s;%s;%d;%s;%d;%s;%d\n" name fun depth (show t) ruleId (show rule) count
        Nothing -> return ()

type Rank = Int

classificationSetup :: ((Rank -> Grammar -> Analysis) -> IO ()) -> IO ()
classificationSetup k =
  withFile "classification.csv" WriteMode $ \csv -> do
    hPrintf csv "name;fun;depth;rank;term;class\n"
    k $ \maxRank grammar name fun depth selected -> measure "Classification" $ do
      let relevant = termsOfRankN maxRank grammar
      forM_ ([1..maxRank]::[Int]) $ \rank -> do
        let selectedN = H.filter (\t -> W.height t == rank) selected
            relevantN = H.filter (\t -> W.height t == rank) relevant
            truePositive = ("true_positive", relevantN `H.intersection` selectedN)
            falsePositive = ("false_positive", selectedN `H.difference` relevantN)
            falseNegative = ("false_negative", relevantN `H.difference` selectedN)
        forM_ [truePositive, falseNegative, falsePositive] $ \(klass,terms) ->
          forM_ terms $ \t ->
            hPrintf csv "%s;%s;%d;%d;%s;%s\n" name fun depth rank (show t) (klass :: String)

measure :: String -> IO () -> IO ()
measure analysisName action = do
  (m,_) <- CM.measure (CT.nfIO action) 1
  printf "- %s: %s\n" analysisName (CM.secs (CT.measCpuTime m))

type Analysis = String -> String -> Int -> HashSet W.Term -> IO ()

caseStudy :: String -> String -> Int -> Analysis -> IO ()
caseStudy name function maxDepth analysis = do
  printf "------------------ case study: %s ----------------------\n" name
  file <- T.readFile =<< getDataFileName (printf "case-studies/%s/%s.aterm" name name)
  case parseModule =<< parseATerm file of
    Left e -> fail (show e)
    Right module_ ->
      forM_ ([1..maxDepth]::[Int]) $ \depth -> do

        let res = H.fromList $ toList $ filterResults
                $ W.eval depth (stratEnv module_) (Call (fromString function) [] []) (W.Wildcard,M.empty)

        (m,_) <- CM.measure (CT.nfIO (return res)) 1
        printf "function: %s, recursion depth: %d, results: %d, time: %s\n" function depth (H.size res) (CM.secs (CT.measCpuTime m))
        analysis name function depth res
        putStrLn "\n"
 where
   filterResults = fmap (\r -> case r of Success (t,_) -> t; Fail -> error "")
                . S.filter (\r -> case r of Success _ -> True; _ -> False)
