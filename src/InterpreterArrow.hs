module InterpreterArrow where

-- import           Prelude hiding ((.),id,fail)
    
-- import           Syntax (StratEnv,HasStratEnv(..))
-- import           Signature hiding (Top)

-- import           Control.Arrow hiding ((<+>))
-- import           Control.Arrow.Join
-- import           Control.Arrow.Try
-- import           Control.Arrow.Fix
-- import           Control.Category
-- import           Control.Monad hiding (fail)
-- import           Control.Monad.Reader hiding (fail)
-- import           Control.Monad.State hiding (fail)
-- import           Control.Monad.Fail hiding (fail)

-- import qualified Data.HashMap.Lazy as M
-- import           Data.Hashable
-- import           Data.Monoid
-- import           Data.Order
-- import           Data.Powerset (Deduplicate(..))
-- import           Data.PowersetResult (PowersetResult)
-- import qualified Data.PowersetResult as P
-- import           Data.Result (Result)
-- import qualified Data.Result as R
-- import           Data.Foldable
-- import           Data.Stack
-- import           Data.Term
-- import           Data.TermEnv
-- import           Data.TypedResult (TypedResult,TypeError(..))
-- import qualified Data.TypedResult as T
-- import           Data.UncertainResult (UncertainResult)

-- newtype Interp m a b = Interp { runInterp :: a -> m b }

-- instance Monad m => Category (Interp m) where
--   id = Interp $ \a -> return a
--   Interp f . Interp g = Interp $ g >=> f

-- instance Monad m => Arrow (Interp m) where
--   arr f = Interp (\_ (a,e) -> return (f a, e))
--   first (Interp f) = Interp $ \r ((a,b),e) -> fmap (\(c,e') -> ((c,b),e')) (f r (a,e))
--   second (Interp f) = Interp $ \r ((a,b),e) -> fmap (\(c,e') -> ((a,c),e')) (f r (b,e))
--   Interp f *** Interp g = Interp $ \r ((a,b),e) -> do
--     (c,e')  <- f r (a,e)
--     (d,e'') <- g r (b,e')
--     return ((c,d),e'')
--   Interp f &&& Interp g = Interp $ \r (a,e) -> do
--     (b,e')  <- f r (a,e)
--     (c,e'') <- g r (a,e')
--     return ((b,c),e'')

-- instance Monad m => ArrowChoice (Interp m) where
--   left (Interp f) = Interp $ \r (a,e) -> case a of
--     Left b -> first Left <$> f r (b,e)
--     Right c -> return (Right c,e)
--   right (Interp f) = Interp $ \r (a,e) -> case a of
--     Left c -> return (Left c,e)
--     Right b -> first Right <$> f r (b,e)
--   Interp f +++ Interp g = Interp $ \r (a,e) -> case a of
--     Left b -> first Left  <$> f r (b,e)
--     Right c -> first Right <$> g r (c,e)
--   Interp f ||| Interp g = Interp $ \r (a,e) -> case a of
--     Left b -> f r (b,e)
--     Right c -> g r (c,e)

-- instance Monad m => ArrowApply (Interp m) where
--   app = Interp $ \r ((f,b),e) -> runInterp f r (b,e)

-- instance MonadReader StratEnv m => HasStratEnv (Interp m) where
--   readStratEnv = Interp $ \r (_,e) -> return (r,e)
--   localStratEnv r (Interp f) = Interp $ \_ (a,e) -> f r (a,e)

-- instance Monad m => ArrowFix (Interp r (ConcreteTermEnv t) m) where
--   fixA n f = f (fixA n f)

-- instance Monad m => ArrowFix (Interp r (AbstractTermEnv t) m) where
--   fixA n f
--     | n <= 0    = top
--     | otherwise = f (fixA (n-1) f)

-- instance (MonadState (ConcreteTermEnv t) m,
--           IsTerm t (Interp m),
--           ArrowTry (Interp m)) =>
--   IsTermEnv (ConcreteTermEnv t) t (Interp m) where
--   getTermEnv = Interp $ \_ (_,e) -> return (e,e)
--   putTermEnv = Interp $ \_ (e,_) -> return ((),e)
--   lookupTermVar f g = proc v -> do
--     ConcreteTermEnv env <- getTermEnv -< ()
--     case M.lookup v env of
--       Just t -> f -< t
--       Nothing -> g -< ()
--   insertTerm = proc (v,t) -> do
--     ConcreteTermEnv env <- getTermEnv -< ()
--     putTermEnv -< ConcreteTermEnv (M.insert v t env)
--   deleteTermVars = proc vars -> do
--     ConcreteTermEnv e <- getTermEnv -< ()
--     putTermEnv -< ConcreteTermEnv (foldr' M.delete e vars)
--   unionTermEnvs = arr (\(vars, ConcreteTermEnv e1, ConcreteTermEnv e2) ->
--     ConcreteTermEnv (M.union e1 (foldr' M.delete e2 vars)))

-- instance (MonadState (AbstractTermEnv t) m, Lattice t,
--           IsAbstractTerm t (Interp m),
--           ArrowTry (Interp m),
--           ArrowJoin (Interp m)) =>
--   IsTermEnv (AbstractTermEnv t) t (Interp m) where
--   getTermEnv = Interp $ \_ (_,e) -> return (e,e)
--   putTermEnv = Interp $ \_ (e,_) -> return ((),e)
--   lookupTermVar f g = proc v -> do
--     AbstractTermEnv env <- getTermEnv -< ()
--     case M.lookup v env of
--       Just t -> f -< t
--       Nothing -> (f <<< wildcard) <+> g -< ()
--   insertTerm = proc (v,t) -> do
--     AbstractTermEnv env <- getTermEnv -< ()
--     putTermEnv -< AbstractTermEnv (M.insert v t env)
--   deleteTermVars = proc vars -> do
--     AbstractTermEnv e <- getTermEnv -< ()
--     putTermEnv -< AbstractTermEnv (foldr' M.delete e vars)
--   unionTermEnvs = arr (\(vars,AbstractTermEnv e1, AbstractTermEnv e2) ->
--     AbstractTermEnv (M.union e1 (foldr' M.delete e2 vars)))

-- instance MonadFail m => ArrowTry (Interp m) where
--   fail = Interp $ \_ -> fail
--   try (Interp f) (Interp g) (Interp h) = Interp $ \a ->
--     case f e a of
--       R.Success b -> g e b 
--       R.Fail -> h e a

-- instance ArrowJoin (Interp Result) where
--   Interp f <+> Interp g = Interp $ \r a -> f r a <> g r a 
--   alternatives = Interp $ \_ (as,e) -> (,e) <$> msum (fmap return as)

-- instance (Monad m, PreOrd (m b)) => PreOrd (Interp m a b) where
--   Interp f ⊑ Interp g = forall $ \a -> f a ⊑ g a

-- instance (Monad m, PartOrd (m b)) => PartOrd (Interp m a b)

-- instance (Monad m, Lattice (m b)) => Lattice (Interp m a b) where
--   Interp f ⊔ Interp g = Interp $ \r x -> f r x ⊔ g r x

-- instance (Monad m, BoundedLattice (m b)) => BoundedLattice (Interp m a b) where
--   top = Interp (\_ _ -> top)

-- forall :: a -> Bool
-- forall = undefined

-- instance HasStack (Interp m) where
--   stackPush _ c = c

-- instance Lattice s r => ArrowTry (Interp r s UncertainResult) where
--   fail = Interp $ \_ _ -> U.Fail
--   try (Interp f) (Interp g) (Interp h) = Interp $ \r (a,e) ->
--     case f r (a,e) of
--       U.Success b -> g r b 
--       U.SuccessOrFail b' -> join $ uncomplete . fst <$> runInterp (⊔) r ((complete (g r b'), complete (h r (a,e))),e)
--       U.Fail -> h r (a,e)



-- uncomplete :: UncertainResult (Complete x,e) -> UncertainResult (x, e)
-- uncomplete c = case c of
--   U.Success (Complete x,e) -> U.Success (x,e)
--   U.SuccessOrFail (Complete x,e) -> U.SuccessOrFail (x,e)
--   _ -> U.Fail
      

-- instance Lattice s (Interp r s UncertainResult) => ArrowJoin (Interp r s UncertainResult) where
--   Interp f <+> Interp g = Interp $ \r x@(_,e) ->
--     join $ uncomplete . fst <$> runInterp (⊔) r ((complete (f r x), complete (g r x)),e)
--   alternatives = Interp $ \_ (as,e) -> (,e) <$> msum (fmap return as)

-- instance Deduplicate (Interp r s Result) where
--   dedup f = f

-- instance Deduplicate (Interp r s UncertainResult) where
--   dedup f = f

-- instance ArrowTry (Interp r s PowersetResult) where
--   fail = Interp $ \_ _ -> P.PowRes (return R.Fail)
--   try (Interp f) (Interp g) (Interp h) = Interp $ \r a -> P.PowRes $ do
--     b <- P.unPowRes $ f r a
--     case b of
--       R.Success b' -> P.unPowRes $ g r b'
--       R.Fail -> P.unPowRes $ h r a

-- instance ArrowJoin (Interp r s PowersetResult) where
--   Interp f <+> Interp g = Interp $ \r x -> f r x `P.union` g r x
--   alternatives = Interp $ \_ (as,e) -> P.fromFoldable (fmap (return . (,e)) as)

-- instance TypeError (Interp r s PowersetResult) where
--   typeError = Interp $ \_ _ -> mempty

-- instance (Eq s, Hashable s) => Deduplicate (Interp r s PowersetResult) where
--   dedup (Interp f) = Interp $ \r a -> P.dedup' $ f r a

-- instance ArrowTry (Interp r s TypedResult) where
--   fail = Interp $ \_ _ -> T.Fail
--   try (Interp f) (Interp g) (Interp h) = Interp $ \e a ->
--     case f e a of
--       T.Success b -> g e b 
--       T.Fail -> h e a
--       T.TypeError t -> T.TypeError t

-- instance ArrowJoin (Interp r s TypedResult) where
--   Interp f <+> Interp g = Interp $ \x -> f x `mappend` g x
--   alternatives = Interp $ \_ (as,e) -> (,e) <$> msum (fmap return as)

-- instance TypeError (Interp r s TypedResult) where
--   typeError = Interp $ \_ (e,_) -> T.TypeError e
