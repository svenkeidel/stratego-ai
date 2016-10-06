{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (fail)

import Syntax
import Interpreter hiding (try)
import qualified ConcreteSemantics as C
import qualified NaiveSemantics as N

import Control.Monad hiding (fail)

import qualified Data.Map as M

f, g :: [TermV] -> TermV
f = ConsV "f"
g = ConsV "g"

x,y :: TermV
x = VarV "x"
y = VarV "y"

f', g' :: [C.Term] -> C.Term
f' = C.Cons "f"
g' = C.Cons "g"

c',d' :: C.Term
c' = C.Cons "c" []
d' = C.Cons "d" []

f'', g'' :: [N.Term] -> N.Term
f'' = N.Cons "f"
g'' = N.Cons "g"

c'',d'' :: N.Term
c'' = N.Cons "c" []
d'' = N.Cons "d" []

p1 :: Strat
p1 = Scope ["x","y"] (Match (f [x,y]) `Seq` Build (g [y,x]))

p2 :: Strat
p2 = Scope ["x"] (Match (f [x,x]) `Seq` Build (g [x,x]))

p3 :: Strat
p3 = topdown (try p1)

try :: Strat -> Strat
try s = LeftChoice s Id

topdown :: Strat -> Strat
topdown s = Rec "x" (Seq s (All (Var "x")))

bottomup :: Strat -> Strat
bottomup s = Rec "x" (Seq (All (Var "x")) s)

main :: IO ()
main = do
  print $ runInterp (C.interp p1 (f' [c',d'])) M.empty M.empty
  print $ runInterp (N.interp p1 (f'' [c'',d''])) M.empty M.empty
  print $ runInterp (N.interp p1 N.Hole) M.empty M.empty
  print $ runInterp (C.interp p2 (f' [c',d'])) M.empty M.empty
  print $ runInterp (N.interp p2 (f'' [c'',d''])) M.empty M.empty
  print $ runInterp (N.interp p2 N.Hole) M.empty M.empty
  print $ runInterp (C.interp (topdown (try p1)) (f' [f' [c', d'], d'])) M.empty M.empty
  print $ runInterp (C.interp (bottomup (try p1)) (f' [f' [c', d'], d'])) M.empty M.empty
  print $ take 5 $ N.unMultilpe $ runInterp (N.interp (topdown (try p1)) N.Hole) M.empty M.empty
