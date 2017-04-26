{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Syntax where

import           Prelude hiding (maybe)

import           ATerm

import           Control.DeepSeq
import           Control.Monad.Except

import           Data.Hashable
import           Data.Text (Text,pack,unpack)
import qualified Data.Text as T
import           Data.List (intercalate)
import           Data.String (IsString(..))
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Set (Set)
import qualified Data.Set as S

import           Test.QuickCheck hiding (subterms)

newtype Constructor = Constructor Text deriving (Eq,Ord,IsString,Hashable,NFData)
newtype TermVar = TermVar Text deriving (Eq,Ord,Hashable)
newtype StratVar = StratVar Text deriving (Eq,Ord,IsString,Hashable)

data TermPattern
  = Cons Constructor [TermPattern]
  | Explode TermPattern TermPattern
  | Var TermVar
  | StringLiteral Text
  | NumberLiteral Int
  deriving (Eq)

data Module = Module Signature Strategies deriving (Show,Eq)
type Signature = HashMap Constructor FunType
data Sort = Sort Text [Sort] | SortVar Text deriving (Show,Eq)
data FunType = FunType [Sort] Sort deriving (Show,Eq)

type Strategies = HashMap StratVar Strategy
data Strategy = Strategy [StratVar] [TermVar] Strat deriving (Show,Eq)
data Closure = Closure Strategy StratEnv deriving (Eq)
type StratEnv = HashMap StratVar Closure
data Strat
  = Fail
  | Id
  | Seq Strat Strat
  | GuardedChoice Strat Strat Strat
  | One Strat
  | Some Strat
  | All Strat
  | Match TermPattern
  | Build TermPattern
  | Scope [TermVar] Strat
  | Call StratVar [Strat] [TermVar]
  | Let [(StratVar,Strategy)] Strat
  deriving (Eq)

leftChoice :: Strat -> Strat -> Strat
leftChoice f = GuardedChoice f Id

stratEnv :: Module -> StratEnv
stratEnv (Module _ senv) = fmap (`Closure` M.empty) senv

patternVars :: TermPattern -> Set TermVar
patternVars t = case t of
  Cons _ ts -> S.unions $ patternVars <$> ts
  Explode f l -> patternVars f `S.union` patternVars l
  Var x -> S.singleton x
  _ -> S.empty

patternVars' :: TermPattern -> [TermVar]
patternVars' = S.toList . patternVars

parseModule :: MonadError String m => ATerm -> m Module
parseModule t = case t of
  ATerm "Specification" [List [ATerm "Signature" [List sig], ATerm "Strategies" [List strats]]] ->
    Module <$> parseSignature sig <*> (M.fromList <$> traverse parseStrategy strats)
  _ -> throwError $ "unexpected input while parsing module from aterm: " ++ show t

parseSignature :: MonadError String m => [ATerm] -> m Signature
parseSignature t = case t of
  ATerm "Constructors" [List constrs]:rest -> do
    sig <- M.fromList <$> traverse parseConstructor constrs
    sig' <- parseSignature rest
    return $ M.union sig sig'
  [] -> return M.empty
  _ -> throwError $ "unexpected input while parsing signature from aterm: " ++ show t

parseConstructor :: MonadError String m => ATerm -> m (Constructor,FunType)
parseConstructor t = case t of
  ATerm "OpDecl" [String con, body] -> do
    typ <- parseFunType body
    return (Constructor con,typ)
  _ -> throwError $ "unexpected input while parsing constructor from aterm: " ++ show t  

parseFunType :: MonadError String m => ATerm -> m FunType
parseFunType t = case t of
  ATerm "FunType" [List args,res] -> FunType <$> traverse parseSort args <*> parseSort res
  ATerm "ConstType" _ -> FunType <$> pure [] <*> parseSort t
  _ -> throwError $ "unexpected input while parsing funtype from aterm: " ++ show t

parseSort :: MonadError String m => ATerm -> m Sort
parseSort t = case t of
  ATerm "ConstType" [t'] -> parseSort t'
  ATerm "Sort" [String sortName, List params] ->
    Sort sortName <$> traverse parseSort params
  ATerm "SortVar" [String v] ->
    return $ SortVar v
  _ -> throwError $ "unexpected input while parsing sort from aterm: " ++ show t

parseStrategy :: MonadError String m => ATerm -> m (StratVar,Strategy)
parseStrategy strat = case strat of
  ATerm "SDefT" [String name, List stratVars, List termVars, body] -> do
    str <- Strategy <$> parseStratVars stratVars
                    <*> parseTermVars termVars
                    <*> parseStrat body
    return (StratVar name, str)
  ATerm "ExtSDef" [String name, List termVars, List stratVars] -> do
    str <- Strategy <$> parseStratVars stratVars
                    <*> parseTermVars termVars
                    <*> return Id
    return (StratVar name, str)
  _ -> throwError $ "unexpected input while parsing strategy from aterm: " ++ show strat

parseStratVars :: MonadError String m => [ATerm] -> m [StratVar]
parseStratVars vars =
  forM vars $ \var -> case var of
    ATerm "VarDec" [String name, _] -> return $ StratVar name
    ATerm _ [String x] -> return $ StratVar x
    _ -> throwError $ "unexpected input while parsing strategy variables from aterm: " ++ show var

parseTermVar :: MonadError String m => ATerm -> m TermVar
parseTermVar var = case var of 
  ATerm "VarDec" [String name, _] -> return $ TermVar name
  ATerm "Var" [String name] -> return $ TermVar name
  ATerm "Wld" [] -> return $ TermVar "_"
  ATerm "Str" [String ""] -> return $ TermVar "_"
  String name -> return $ TermVar name
  _ -> throwError $ "unexpected input while parsing term variables from aterm: " ++ show var

parseTermVars :: MonadError String m => [ATerm] -> m [TermVar]
parseTermVars = traverse parseTermVar

parseStrat :: MonadError String m => ATerm -> m Strat
parseStrat t = case t of
  ATerm "Id" [] -> return Id
  ATerm "Fail" [] -> return Fail
  ATerm "Seq" [e1, e2] -> Seq <$> parseStrat e1 <*> parseStrat e2
  ATerm "GuardedLChoice" [e1, e2, e3] -> GuardedChoice <$> parseStrat e1 <*> parseStrat e2 <*> parseStrat e3
  ATerm "All" [e] -> All <$> parseStrat e
  ATerm "Some" [e] -> Some <$> parseStrat e
  ATerm "One" [e] -> One <$> parseStrat e
  ATerm "Match" [tp] -> Match <$> parseTermPattern tp
  ATerm "Build" [tp] -> Build <$> parseTermPattern tp
  ATerm "Scope" [List vars, e] -> Scope <$> parseTermVars vars <*> parseStrat e
  ATerm "CallT" [ATerm "SVar" [String svar], List stratArgs, List termArgs] ->
    Call (StratVar svar) <$> traverse parseStrat stratArgs <*> parseTermVars termArgs
  ATerm "Let" [List strats, body] ->
    Let <$> traverse parseStrategy strats <*> parseStrat body
  _ -> throwError $ "unexpected input while parsing strategy from aterm: " ++ show t

parseTermPattern :: MonadError String m => ATerm -> m TermPattern
parseTermPattern p = case p of
  ATerm "Anno" [p1,_] -> parseTermPattern p1 -- Ignore annotations
  ATerm "Op" [String con, List subterms] ->
    Cons (Constructor con) <$> traverse parseTermPattern subterms
  ATerm "Str" [String con] -> return $ Cons (Constructor con) []
  ATerm "Var" _ -> Var <$> parseTermVar p
  ATerm "Wld" _ -> Var <$> parseTermVar p
  ATerm "Explode" [p1,p2] -> Explode <$> parseTermPattern p1 <*> parseTermPattern p2
  _ -> throwError $ "unexpected input while parsing term pattern from aterm: " ++ show p


instance Show Constructor where
  show (Constructor c) = unpack c

instance Arbitrary TermVar where
  arbitrary = TermVar . T.singleton <$> choose ('a','z')

instance Show TermVar where
  show (TermVar x) = unpack x

instance IsString TermVar where
  fromString = TermVar . pack

instance IsString TermPattern where
  fromString = Var . fromString

instance Hashable TermPattern where
  hashWithSalt s x = case x of
    Cons c ts -> s `hashWithSalt` (0::Int) `hashWithSalt` c `hashWithSalt` ts
    Explode t1 t2 -> s `hashWithSalt` (1::Int) `hashWithSalt` t1 `hashWithSalt` t2
    Var tv -> s `hashWithSalt` (2::Int) `hashWithSalt` tv
    StringLiteral l -> s `hashWithSalt` (3::Int) `hashWithSalt` l
    NumberLiteral l -> s `hashWithSalt` (5::Int) `hashWithSalt` l

instance Show TermPattern where
  show (Cons c ts) = show c ++ if null ts then "" else show ts
  show (Var x) = show x
  show (Explode f xs) = show f ++ "#(" ++ show xs ++ ")"
  show (StringLiteral s) = show s
  show (NumberLiteral n) = show n

instance IsString Strat where
  fromString s = Call (fromString s) [] []

instance Hashable Strat where
  hashWithSalt s x = case x of
    Fail -> s `hashWithSalt` (0::Int)
    Id -> s `hashWithSalt` (1::Int)
    Seq e1 e2 -> s `hashWithSalt` (2::Int) `hashWithSalt` e1 `hashWithSalt` e2
    GuardedChoice e1 e2 e3 -> s `hashWithSalt` (3::Int) `hashWithSalt` e1 `hashWithSalt` e2 `hashWithSalt` e3
    One e -> s `hashWithSalt` (4::Int) `hashWithSalt` e
    Some e -> s `hashWithSalt` (5::Int) `hashWithSalt` e
    All e -> s `hashWithSalt` (6::Int) `hashWithSalt` e
    Match t -> s `hashWithSalt` (7::Int) `hashWithSalt` t
    Build t -> s `hashWithSalt` (8::Int) `hashWithSalt` t
    Scope tv e -> s `hashWithSalt` (9::Int) `hashWithSalt` tv `hashWithSalt` e
    Call sv ss tv -> s `hashWithSalt` (10::Int) `hashWithSalt` sv `hashWithSalt` ss `hashWithSalt` tv
    Let bnds body -> s `hashWithSalt` (11::Int) `hashWithSalt` bnds `hashWithSalt` body

instance Show Strat where
  showsPrec d s0 = case s0 of
    Fail ->
      showString "fail"
    Id ->
      showString "id"
    Seq s1 s2 ->
      showParen (d > seq_prec)
        $ showsPrec seq_prec s1
        . showString "; "
        . showsPrec seq_prec s2
    GuardedChoice s1 s2 s3 ->
      showParen (d > choice_prec)
        $ showsPrec (choice_prec+1) s1
        . showString " < "
        . showsPrec (choice_prec+1) s2
        . showString " + "
        . showsPrec (choice_prec+1) s3
    One s ->
      showParen (d > app_prec)
        $ showString "one "
        . showsPrec (app_prec+1) s
    Some s ->
      showParen (d > app_prec)
        $ showString "some "
        . showsPrec (app_prec+1) s
    All s ->
      showParen (d > app_prec)
        $ showString "all "
        . showsPrec (app_prec+1) s
    Match t ->
      showString "?"
        . showsPrec (app_prec+1) t
    Build t ->
      showString "!"
        . showsPrec (app_prec+1) t
    Scope vars s ->
      showString "{ "
      . showString (intercalate "," (map show vars))
      . showString ": "
      . shows s
      . showString " }"
    Call (StratVar f) ss ts ->
     showString (unpack f)
     . showString "("
     . showString (intercalate "," (map show ss))
     . showString "|"
     . showString (intercalate "," (map show ts))
     . showString ")"
    Let ss body ->
     showString "let "
     . showString (intercalate "," (map show ss))
     . showString " in "
     . shows body
     . showString " end"
    where
      app_prec = 10
      seq_prec = 9
      choice_prec = 8

instance Hashable Strategy where
  hashWithSalt s (Strategy svs tvs body) = s `hashWithSalt` svs `hashWithSalt` tvs `hashWithSalt` body

instance Show Closure where
  show (Closure s _) = show s

instance Show StratVar where
  show (StratVar x) = unpack x

instance Arbitrary Constructor where
  arbitrary = Constructor <$> arbitraryLetter

arbitraryLetter :: Gen Text
arbitraryLetter = T.singleton <$> choose ('A','Z')

instance Num TermPattern where
  t1 + t2 = Cons "Add" [t1,t2]
  t1 - t2 = Cons "Sub" [t1,t2]
  t1 * t2 = Cons "Mul" [t1,t2]
  abs t = Cons "Abs" [t]
  signum t = Cons "Signum" [t]
  fromInteger = NumberLiteral . fromIntegral

instance Arbitrary TermPattern where
  arbitrary = do
    h <- choose (0,7)
    w <- choose (0,4)
    arbitraryTermPattern h w arbitrary

arbitraryTermPattern :: Int -> Int -> Gen TermVar -> Gen TermPattern
arbitraryTermPattern h w var
  | h == 0 =
      oneof
        [ Cons <$> arbitrary <*> pure []
        , StringLiteral <$> arbitraryLetter
        , NumberLiteral <$> choose (0,9)
        , Var <$> var
        ]
  | otherwise = do
      w' <- choose (0,w)
      oneof
        [ do
          c <- arbitrary
          fmap (Cons c) $ vectorOf w' $ do
            h' <- choose (0,h-1)
            arbitraryTermPattern h' w var
        , Explode <$> arbitraryStringPattern
                  <*> arbitraryListPattern w'
        ]

  where
    arbitraryStringPattern :: Gen TermPattern
    arbitraryStringPattern =
      oneof
        [ Var <$> var
        , StringLiteral <$> arbitraryLetter
        ]

    arbitraryListPattern :: Int -> Gen TermPattern
    arbitraryListPattern len =
      if len == 0
        then return $ Cons "Nil" []
        else do
          h' <- choose (0,h-1)
          tp <- arbitraryTermPattern h' w var
          tl <- arbitraryListPattern (len-1)
          return $ Cons "Cons" [tp,tl]
