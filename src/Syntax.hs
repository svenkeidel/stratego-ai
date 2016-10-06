module Syntax where

import Prelude hiding (maybe)

import Data.Text (Text)

import Control.Monad.State
import Control.Monad.Reader

type Var = Text

data Strat
    = Test Strat
    | Neg Strat
    | Fail
    | Id
    | Seq Strat Strat
    | Choice Strat Strat
    | LeftChoice Strat Strat
    | Rec Var Strat
    | Var Var
    | Path Int Strat
    | Cong Text [Strat]
    | One Strat
    | Some Strat
    | All Strat
    | Match TermV
    | Build TermV
    | Where Strat
    | Scope [Var] Strat
    deriving Show

type Constructor = Text

data TermV = ConsV Text [TermV]
           | VarV Var
  deriving Show


class MonadMaybe m where
  maybe :: m a -> (Maybe a -> m b) -> m b

instance MonadMaybe Maybe where
  maybe m f = f m

instance MonadMaybe m => MonadMaybe (ReaderT r m) where
  maybe rd f = ReaderT
             $ \r -> maybe (runReaderT rd r)
             $ \a -> runReaderT (f a) r

instance MonadMaybe m => MonadMaybe (StateT s m) where
  maybe st f = StateT
             $ \s -> maybe (runStateT st s)
             $ \m -> case m of
                       Just (a,s') -> runStateT (f (Just a)) s'
                       Nothing -> runStateT (f Nothing) s
