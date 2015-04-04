{-# LANGUAGE TupleSections #-}



-- module

module RCL.RTM where



-- imports

import           Control.Monad.Error
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State

import           RCL.Query
import           RCL.Response
import           RCL.Services
import           RCL.Session



-- exported types

type RTMT m = ReaderT (Services m) (StateT Session (ErrorT Failure m))
type RTM    = RTMT Identity



-- exported functions

runRTM :: Monad m => Services m -> APIKey -> Secret -> RTMT m a -> m (Either Failure a)
runRTM c k s r = runErrorT $ evalStateT (runReaderT r c) $ Session k s "" ""

liftRTM :: Monad m => m a -> RTMT m a
liftRTM = lift . lift . lift

liftERTM :: Monad m => m (Either Failure a) -> RTMT m a
liftERTM = lift . lift . ErrorT

runQuery :: (Monad m, Functor m) => URL -> QueryBuilder -> RTMT m Response
runQuery u b = do
  q <- asks querant
  s <- get
  r <- liftRTM $ q (create s u b)
  responseM r
