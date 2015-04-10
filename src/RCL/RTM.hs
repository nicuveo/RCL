{-# LANGUAGE TupleSections #-}



-- module

module RCL.RTM (
  RTMT,
  RTM,
  runRTM,
  liftRTM,
  liftERTM,
  config,
  services,
  session,
  call,
  nonAuthCall,
  silent,
  ) where



-- imports

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State

import           RCL.Config
import           RCL.Query
import           RCL.Response
import           RCL.Services
import           RCL.Session



-- exported types

type RTMT m = ReaderT (Config, Services m) (StateT Session (ErrorT Failure m))
type RTM    = RTMT Identity



-- exported functions

runRTM :: Monad m => Config -> Services m -> RTMT m a -> m (Either Failure a)
runRTM c s r = runErrorT $ flip evalStateT emptySession $ runReaderT r (c, s)

liftRTM :: Monad m => m a -> RTMT m a
liftRTM = lift . lift . lift

liftERTM :: Monad m => m (Either Failure a) -> RTMT m a
liftERTM = lift . lift . ErrorT

config :: (Monad m, Functor m) => RTMT m Config
config = fst <$> ask

services :: (Monad m, Functor m) => RTMT m (Services m)
services = snd <$> ask

session :: (Monad m, Functor m) => RTMT m Session
session = get

call :: (Monad m, Functor m) => QueryBuilder -> RTMT m Response
call q = doCall $ format >=> q >=> auth >=> sign

nonAuthCall :: (Monad m, Functor m) => QueryBuilder -> RTMT m Response
nonAuthCall q = doCall $ format >=> q >=> sign

silent :: (Monad m, Functor m) => RTMT m a -> RTMT m ()
silent q = void q `catchError` const (return ())



-- internal functions

doCall :: (Monad m, Functor m) => QueryBuilder -> RTMT m Response
doCall b = do
  s <- get
  c <- config
  q <- asks $ querant . snd
  r <- liftRTM $ q (create c s (restURL c) b)
  responseM r

-- atomic :: (Monad m, Functor m) => RTMT m a -> RTMT m a
-- atomic a = do
--   startNewTimeline
--   r <- a `catchError` onFail
--   onSuccess r
--
--   where onFail f = do
--           silent revertTimeline
--           throwError f
--             stopTimeline
--   return result
