{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Prelude hiding (log)

import           Grammar.RegularTreeGrammar
import           Syntax hiding (Fail)
import qualified WildcardSemantics as W
import qualified WildcardSemanticsDelayed as W
import qualified Soundness as U

import qualified Pretty.Haskell as H
import qualified Pretty.JavaScript as J
import qualified Pretty.PCF as P
import           Pretty.Results

import           Paths_system_s

import           Control.Monad
import qualified Criterion.Measurement as CM
import qualified Criterion.Types as CT

import           Data.ATerm
import           Data.Foldable
import qualified Data.HashMap.Lazy as M
import           Data.HashSet (HashSet)
import qualified Data.HashSet as H
import           Data.Hashable
import           Data.Maybe
import           Data.Monoid
import           Data.Result
import qualified Data.Sequence as S
import           Data.String
import qualified Data.Text.IO as TIO
import           Data.Term (TermUtils)
import qualified Data.Term as T
import           Data.TermEnv
import           Data.Powerset(unPow)

import           System.IO

import           Text.PrettyPrint hiding (sep,(<>))
import           Text.Printf

main :: IO ()
main =
  sizeAnalysisSetup $ \sizeAnalysis ->
  heightAnalysisSetup $ \heightAnalysis -> do
  -- wittnessAnalysisSetup $ \wittnessAnalysis ->
  -- ruleInvocationsAnalysisSetup $ \ruleInvocationsAnalysis ->
  -- classificationSetup $ \classification -> do
    CM.initializeTime

    -- activate $ caseStudy "arrows" "desugar_arrow_0_0" 4 $
    --   prettyPrint H.ppHaskell <>
    --   sizeAnalysis <>
    --   heightAnalysis
      -- wittnessAnalysis

  --   activate $ caseStudy "cca" "norm_0_0" 5 $
  --     prettyPrint H.ppHaskell <>
  --     sizeAnalysis <>
  --     heightAnalysis <>
  --     wittnessAnalysis

    activate $ caseStudy "arith" "eval_0_0" 3 $
      prettyPrint P.ppPCF <>
      sizeAnalysis <>
      heightAnalysis

    -- activate $ caseStudy "pcf" "eval_0_0" 4 $
    --   prettyPrint P.ppPCF <>
    --   sizeAnalysis <>
    --   heightAnalysis
  --     wittnessAnalysis <>
  --     ruleInvocationsAnalysis pcfEvalGrammar <>
  --     classification pcfEvalGrammar 4

  --   activate $ caseStudy "pcf" "check_eval_0_0" 5 $
  --     prettyPrint P.ppPCF <>
  --     sizeAnalysis <>
  --     heightAnalysis <>
  --     wittnessAnalysis <>
  --     ruleInvocationsAnalysis pcfCheckEvalGrammar <>
  --     classification pcfCheckEvalGrammar 4

  --   activate $ caseStudy "go2js" "generate_js_ast_0_0" 4 $
  --     prettyPrint J.tryPPJS <>
  --     sizeAnalysis <>
  --     heightAnalysis <>
  --     wittnessAnalysis

  where
    activate :: IO () -> IO ()
    activate cs = cs

    deactivate :: IO () -> IO ()
    deactivate _ = return ()

prettyPrint :: (W.Term -> Doc) -> Analysis W.Term
prettyPrint pprint _ _ _ res =
  if H.size res <= 200
     then print $ ppResults pprint (toList res)
     else printf "Output ommited because of result set contains %d element\n" (H.size res)

sizeAnalysisSetup :: (Show t, TermUtils t) => (Analysis t -> IO ()) -> IO ()
sizeAnalysisSetup k =
  withFile "size.csv" WriteMode $ \csv -> do
    hPrintf csv "name;fun;depth;term;size\n"
    k $ \name fun depth res -> measure "Size Analysis" $ forM_ res $ \t ->
      hPrintf csv "%s;%s;%d;%s;%d\n" name fun depth (show t) (T.size t)

heightAnalysisSetup :: (Show t, TermUtils t) => (Analysis t -> IO ()) -> IO ()
heightAnalysisSetup k =
  withFile "height.csv" WriteMode $ \csv -> do
    hPrintf csv "name;fun;depth;term;height\n"
    k $ \name fun depth res -> measure "Height Analysis" $ forM_ res $ \t ->
        hPrintf csv "%s;%s;%d;%s;%d\n" name fun depth (show t) (T.height t)

-- wittnessAnalysisSetup :: (Analysis t -> IO ()) -> IO ()
-- wittnessAnalysisSetup k =
--   withFile "wittness.csv" WriteMode $ \csv -> do
--     hPrintf csv "name;fun;depth;term;wittness\n"
--     k $ \name fun depth res -> measure "Wittness Analysis" $ forM_ res $ \t ->
--         hPrintf csv "%s;%s;%d;%s;%s\n" name fun depth (show t) (show (U.isWittness (t,res)))

-- ruleInvocationsAnalysisSetup :: ((Grammar -> Analysis t) -> IO ()) -> IO ()
-- ruleInvocationsAnalysisSetup k =
--   withFile "rule.csv" WriteMode $ \csv -> do
--     hPrintf csv "name;fun;depth;term;ruleId;rule;invocation\n"
--     k $ \grammar name fun depth res -> measure "Rule Invocations Analysis" $ forM_ res $ \t ->
--       case summary grammar t of
--         Just s -> forM_ (M.toList s `zip` [(1::Int)..]) $ \((rule,count),ruleId) ->
--           hPrintf csv "%s;%s;%d;%s;%d;%s;%d\n" name fun depth (show t) ruleId (show rule) count
--         Nothing -> return ()

-- type Distance = Int

-- | Classifies terms as true and false, positive or negatives.
--
-- True-Positive: Terms in the analysis result that are part of the output language of the program transformation
-- False-Positive: Terms in the analysis result that are *not* part of the output language of the program transformation
-- True-Negative: Terms that occur neither in the output language of the program transformation nor in the analysis result
-- False-Negative: Terms in output language of the program transformation that do not occur in the analysis result
-- classificationSetup :: ((Grammar -> Distance -> Analysis t) -> IO ()) -> IO ()
-- classificationSetup k =
--   withFile "classification.csv" WriteMode $ \csv -> do
--     hPrintf csv "name;fun;depth;sample_distance;term;height;size;distance_sum;class\n"
--     k $ \grammar sampleDistance name fun depth analysis -> measure "Classification" $ do
--       let relevant = termsOfDistance sampleDistance grammar
--           selected = H.delete W.Wildcard analysis
--           (truePositive,falsePositive) = partition (\s -> any (\r -> s `wittnesses` r) relevant) selected
--           (_,           falseNegative) = partition (\r -> any (\s -> s `wittnesses` r) selected) relevant
--       forM_ [("true_positive", truePositive), ("false_positive", falsePositive), ("false_negative", falseNegative)] $ \(klass,terms) ->
--         forM_ terms $ \t ->
--           hPrintf csv "%s;%s;%d;%d;%s;%d;%d;%d;%s\n"
--             name fun depth sampleDistance (show t) (T.height t) (T.size t) (distanceSum grammar t `orElse` 0) (klass :: String)
--   where
--     partition :: (Eq a, Hashable a) => (a -> Bool) -> HashSet a -> (HashSet a, HashSet a)
--     partition predicate = H.foldl' (\(pos,neg) t -> if predicate t then (H.insert t pos,neg) else (pos,H.insert t neg))
--                                    (H.empty,H.empty)

-- wittnesses :: W.Term -> W.Term -> Bool
-- wittnesses W.Wildcard W.Wildcard = True
-- wittnesses W.Wildcard (W.Cons _ []) = True
-- wittnesses W.Wildcard (W.StringLiteral _) = True
-- wittnesses W.Wildcard (W.NumberLiteral _) = True
-- wittnesses (W.Cons c ts) (W.Cons c' ts') = c == c' && length ts == length ts' && and (zipWith wittnesses ts ts')
-- wittnesses (W.StringLiteral s) (W.StringLiteral s') = s == s'
-- wittnesses (W.NumberLiteral n) (W.NumberLiteral n') = n == n'
-- wittnesses _ _ = False

-- for :: Functor f => f a -> (a -> b) -> f b
-- for = flip fmap

-- orElse :: Maybe a -> a -> a
-- orElse = flip fromMaybe

measure :: String -> IO () -> IO ()
measure analysisName action = do
  (m,_) <- CM.measure (CT.nfIO action) 1
  printf "- %s: %s\n" analysisName (CM.secs (CT.measCpuTime m))

type Analysis t = String -> String -> Int -> HashSet t -> IO ()

caseStudy :: String -> String -> Int -> Analysis W.Term -> IO ()
caseStudy name function maxDepth analysis = do
  printf "------------------ case study: %s ----------------------\n" name
  file <- TIO.readFile =<< getDataFileName (printf "case-studies/%s/%s.aterm" name name)
  case parseModule =<< parseATerm file of
    Left e -> fail (show e)
    Right module_ ->
      forM_ ([1..maxDepth]::[Int]) $ \depth -> do

        let res = H.fromList $ toList $ filterResults $ unPow
                $ W.eval depth (stratEnv module_) (Call (fromString function) [] []) (W.Wildcard,AbstractTermEnv M.empty)

        (m,_) <- CM.measure (CT.nfIO (return res)) 1
        printf "function: %s, recursion depth: %d, results: %d, time: %s\n" function depth (H.size res) (CM.secs (CT.measCpuTime m))
        analysis name function depth res
        putStrLn "\n"
 where
   filterResults = fmap (\r -> case r of Success (t,_) -> t; Fail -> error "")
                 . S.filter (\r -> case r of Success _ -> True; _ -> False)
