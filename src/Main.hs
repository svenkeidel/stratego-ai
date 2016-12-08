{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Prelude hiding (fail)

-- import Term
-- import Syntax
-- import Interpreter hiding (try,limit)
-- import ConcreteSemantics (ClosedTerm)
-- import qualified ConcreteSemantics as C
-- import HoleSemantics (PartialTerm)
-- import qualified HoleSemantics as H
-- import Multiple
-- --import GrammarSemantics (Grammar,(~>),grammar)

-- import qualified Data.Map as M
-- import Data.Sequence (Seq)

-- import Text.Printf
-- import Data.String

-- f, g :: [Term a] -> Term a
-- f = Cons "f"
-- g = Cons "g"

-- c, d :: Term a
-- c = Cons "c" []
-- d = Cons "d" []

-- x,y :: IsString a => Term a
-- x = Var "x"
-- y = Var "y"

-- f', g' :: [ClosedTerm] -> ClosedTerm
-- f' = C.Cons "f"
-- g' = C.Cons "g"

-- c',d' :: ClosedTerm
-- c' = C.Cons "c" []
-- d' = C.Cons "d" []

-- f'', g'' :: [PartialTerm] -> PartialTerm
-- f'' = H.Cons "f"
-- g'' = H.Cons "g"

-- c'',d'' :: PartialTerm
-- c'' = H.Cons "c" []
-- d'' = H.Cons "d" []

-- p1 :: Strat
-- p1 = Scope ["x","y"] (Match (f [x,y]) `Seq` Build (g [y,x]))

-- p2 :: Strat
-- p2 = Scope ["x"] (Match (f [x,x]) `Seq` Build (g [x,x]))

-- p3 :: Strat
-- p3 = topdown (try p1)

-- try :: Strat -> Strat
-- try s = LeftChoice s Id

-- topdown :: Strat -> Strat
-- topdown s = Rec "x" (Seq s (All (RecVar "x")))

-- bottomup :: Strat -> Strat
-- bottomup s = Rec "x" (Seq (All (RecVar "x")) s)

-- -- g1 :: Grammar
-- -- g1 = grammar s
-- --        [ s ~> f[Var a,Var b]
-- --        , s ~> c
-- --        , a ~> g[Var a,Var b]
-- --        , b ~> d
-- --        ]
-- --   where
-- --     s = "S"
-- --     a = "A"
-- --     b = "B"

main :: IO ()
main = undefined
    -- do
--   interpC p1 (f' [c',d'])
--   interpH p1 (f'' [c'',d''])
--   interpH p1 H.Hole
--   interpC p2 (f' [c',d'])
--   interpH p2 (f'' [c'',d''])
--   interpH p2 H.Hole
--   interpC (topdown (try p1)) (f' [f' [c', d'], d'])
--   interpC (bottomup (try p1)) (f' [f' [c', d'], d'])
--   interpH (topdown (try p1)) H.Hole
--   interpH (bottomup (try p1)) H.Hole
--   where
--     limit = (0,3)
--     interpC p t =
--       printSummary "concrete" p t
--         (runInterp (C.interp p t) M.empty limit M.empty :: Result (ClosedTerm,C.TermEnv))
--     interpH p t =
--       printSummary "hole" p t
--         (runInterp (H.interp p t) M.empty limit M.empty :: Multiple Seq (PartialTerm,H.TermEnv))

--     printSummary :: (Show a,Show b) => String -> Strat -> a -> b -> IO ()
--     printSummary sem p inp out = do
--        printf "semantics: %s\nprogram:   %s\ninput:     %s\noutput:\n%s\n"
--          sem (show p) (show inp) (show out)
--        putStrLn "------------------------------------------------------------\n"
