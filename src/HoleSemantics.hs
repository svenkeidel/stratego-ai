{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module HoleSemantics where

-- import Prelude hiding (fail,concat,sequence,all,zipWith,(.))
-- import qualified Prelude as P

-- import Term (Term,Constructor,TermVar)
-- import qualified Term as T
-- import Syntax hiding (Fail)
-- import qualified Syntax as S
-- import Interpreter

-- import Control.Monad (ap)
-- import Control.Category
-- import Control.Arrow

-- import Data.Semigroup (Semigroup, (<>))
-- import Data.Map (Map)
-- import qualified Data.Map as M
-- import Data.Sequence (Seq)

-- data PartialTerm
--     = Cons Constructor [PartialTerm]
--     | Hole
--     deriving Eq

-- instance Show PartialTerm where
--   show (Cons c ts) = show c ++ if null ts then "" else show ts
--   show Hole = "_"

-- type Env = Map TermVar PartialTerm

-- data Uncertain a = Success a | SuccessOrFail a | Fail

-- instance Functor Uncertain where
--   fmap f u = case u of
--     Success a -> Success (f a)
--     SuccessOrFail a -> SuccessOrFail (f a)
--     Fail -> Fail

-- instance Applicative Uncertain where
--   pure = return
--   (<*>) = ap

-- instance Monad Uncertain where
--   return = Success
--   f >>= k = case f of
--     Success a -> k a
--     SuccessOrFail a -> case k a of
--       Success b -> SuccessOrFail b
--       SuccessOrFail b -> SuccessOrFail b
--       Fail -> Fail
--     Fail -> Fail

-- plus :: (a -> b -> c) -> a -> b -> Uncertain a -> Uncertain b -> Uncertain c
-- plus f a0 b0 u1 u2 = case (u1,u2) of
--   (Success a, Success b) -> Success (f a b)
--   (Success a, SuccessOrFail b) -> SuccessOrFail (f a b)
--   (SuccessOrFail a,Success b) -> SuccessOrFail (f a b)
--   (Success a, Fail) -> SuccessOrFail (f a b0)
--   (Fail, Success b) -> SuccessOrFail (f a0 b)
--   (SuccessOrFail a, SuccessOrFail b) -> SuccessOrFail (f a b)
--   (SuccessOrFail a, Fail) -> SuccessOrFail (f a b0)
--   (Fail, SuccessOrFail b) -> SuccessOrFail (f a0 b)
--   (Fail, Fail) -> Fail

-- instance (Monoid a) => Semigroup (Uncertain a) where
--   (<>) = plus mappend mempty mempty

-- (>>=>>) :: (a -> Uncertain (Seq b)) -> (b -> Uncertain (Seq c)) -> (a -> Uncertain (Seq c))
-- f >>=>> g = \a -> case f a of
--   Success bs -> join (Success mempty) (fmap g bs)
--   SuccessOrFail bs -> join (SuccessOrFail mempty) (fmap g bs)
--   Fail -> Fail

-- join :: Uncertain (Seq a) -> Seq (Uncertain (Seq a)) -> Uncertain (Seq a)
-- join = foldl (<>)

-- newtype Interp a b = Interp {runInterp :: (a,Env) -> Uncertain (Seq (b,Env))}

-- instance Category Interp where
--   id = Interp (Success . return)
--   f . g = Interp $ runInterp g >>=>> runInterp f

-- cartesian :: Semigroup e => Seq (a,e) -> Seq (b,e) -> Seq ((a,b),e)
-- cartesian as bs = do
--   (a,e) <- as
--   (b,e') <- bs
--   return ((a,b),e <> e')

-- instance Arrow Interp where
--   arr f = Interp (\(a,e) -> Success (return (f a, e)))
--   first f =  Interp $ \((a,b),e) -> (fmap.fmap) (\(c,e') -> ((c,b),e')) (runInterp f (a,e))
--   second f = Interp $ \((a,b),e) -> (fmap.fmap) (\(c,e') -> ((a,c),e')) (runInterp f (b,e))
--   f &&& g = Interp $ \x -> plus cartesian mempty mempty (runInterp f x) (runInterp g x)
--   f *** g = Interp $ \((a,b),e) ->  plus cartesian mempty mempty (runInterp f (a,e)) (runInterp g (b,e))

-- instance ArrowChoice Interp where
--   left f = Interp $ \(a,e) -> case a of
--     Left b -> (fmap.fmap) (first Left) (runInterp f (b,e))
--     Right c -> Success (return (Right c,e))
--   right f = Interp $ \(a,e) -> case a of
--     Left c -> Success (return (Left c,e))
--     Right b -> (fmap.fmap) (first Right) (runInterp f (b,e))
--   f +++ g = Interp $ \(a,e) -> case a of
--     Left b  -> (fmap.fmap) (first Left)  (runInterp f (b,e))
--     Right b -> (fmap.fmap) (first Right) (runInterp g (b,e))

-- instance Try Interp where
--   success = Interp (Success . return)
--   fail = Interp (const Fail)
--   try t s f = Interp $ \(a,e) -> case runInterp t (a,e) of
--     Success xs -> join (Success mempty) (runInterp s <$> xs)
--     Fail -> runInterp f (a,e)
--     SuccessOrFail xs -> join (SuccessOrFail mempty) (runInterp s <$> xs)

-- instance ArrowZero Interp where
--   zeroArrow = Interp (const Fail)

-- instance ArrowPlus Interp where
--   f <+> g = Interp $ \x -> runInterp f x <> runInterp g x

-- instance TermEnv (Map TermVar PartialTerm) Interp where
--   getTermEnv = Interp $ \((),e) -> Success (return (e,e))
--   putTermEnv = Interp $ \(e,_) -> Success (return ((),e))

-- interp :: (Try p, TermEnv Env p,ArrowChoice p,ArrowPlus p) => StratEnv -> Strat -> p PartialTerm PartialTerm
-- interp env s0 = case s0 of
--   Test s -> test (interp env s)
--   Neg s -> neg (interp env s)
--   S.Fail -> fail
--   Id -> success
--   Seq s1 s2 -> sequence (interp env s1) (interp env s2)
--   Choice s1 s2 -> choice (interp env s1) (interp env s2)
--   LeftChoice s1 s2 -> leftChoice (interp env s1) (interp env s2)
--   Rec x s -> recur env x s (`interp` s)
--   RecVar x -> var env x (interp env)
--   Path i s -> lift (path i (interp env s))
--   Cong c ss -> proc t -> case t of
--     Cons c' ts' -> do
--       (c'',ts'') <- cong c (interp env <$> ss) -< (c',ts')
--       returnA -< Cons c'' ts''
--     Hole -> fail <+> interp env (Cong c ss) -< Cons c [Hole | _ <- ss]
--   One s -> lift (one (interp env s))
--   Some s -> lift (some (interp env s))
--   All s -> lift (all (interp env s))
--   Scope xs s -> scope xs (interp env s)
--   Match f -> proc t -> match -< (f,t)
--   Build f -> proc _ -> build -< f

-- match :: (Try p, TermEnv (Map TermVar PartialTerm) p, ArrowChoice p, ArrowPlus p)
--       => p (Term TermVar,PartialTerm) PartialTerm
-- match = proc (f,t) -> case f of
--   T.Var x -> do
--     env <- getTermEnv -< ()
--     case M.lookup x env of
--       Just t' | groundEq t t' -> success -< t
--               | otherwise -> fail <+> success <<< unify -< (t,t')
--       Nothing -> do
--         putTermEnv -< env
--         success -< t
--   T.Cons c ts -> case t of
--     Cons c' ts'
--       | c /= c' || length ts /= length ts' -> fail -< ()
--       | otherwise -> do
--           ts'' <- zipWith match -< (ts,ts')
--           returnA -< Cons c ts''
--     Hole -> do
--       ts'' <- fail <+> zipWith match -< (ts,[Hole | _ <- ts])
--       returnA -< Cons c ts''

-- groundEq :: PartialTerm -> PartialTerm -> Bool
-- groundEq (Cons c ts) (Cons c' ts')
--   | c == c' = P.all (uncurry groundEq) (zip ts ts')
-- groundEq _ _ = False

-- unify :: (Try p,ArrowChoice p) => p (PartialTerm,PartialTerm) PartialTerm
-- unify = proc (t1,t2) -> case (t1,t2) of
--   (Cons c ts,Cons c' ts')
--     | c /= c' || length ts /= length ts' -> fail -< ()
--     | otherwise -> do
--         ts'' <- zipWith unify -< (ts,ts')
--         returnA -< Cons c ts''
--   (Hole, t) -> returnA -< t
--   (t, Hole) -> returnA -< t

-- build :: (Try p,TermEnv (Map TermVar PartialTerm) p, ArrowChoice p) => p (Term TermVar) PartialTerm
-- build = proc f -> case f of
--   (T.Var x) -> do
--     env <- getTermEnv -< ()
--     case M.lookup x env of
--       Nothing -> fail -< ()
--       Just t -> success -< t
--   (T.Cons c ts) -> do
--     ts' <- mapA build -< ts
--     returnA -< Cons c ts'

-- lift :: (Try p,ArrowChoice p,ArrowPlus p)
--      => p (Constructor,[PartialTerm]) (Constructor,[PartialTerm])
--      -> p PartialTerm PartialTerm
-- lift p = proc t -> case t of
--   Cons c ts -> do
--     (c',ts') <- p -< (c,ts)
--     returnA -< Cons c' ts'
--   Hole ->
--     fail <+> success -< Hole
