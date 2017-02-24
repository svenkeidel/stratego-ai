module PrettyPrint where

import           WildcardSemantics
import           Result

import           Data.Foldable(toList)
import           Data.Sequence (Seq)

import           Text.PrettyPrint hiding (sep)


ppResults :: (Term -> Doc) -> Seq (Result (Term,TermEnv)) -> Doc
ppResults ppTerm res = braces
              $ cat
              $ punctuate (comma <> space)
              $ toList
              $ ppResult ppTerm <$> res

ppResult :: (Term -> Doc) -> Result (Term,TermEnv) -> Doc
ppResult ppTerm res = case res of
  Success (t,_) -> ppTerm t
  Fail -> text "Fail"
