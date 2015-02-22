{-# LANGUAGE TupleSections #-}



-- module

module RCL.RTM where



-- imports

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import RCL.Query
import RCL.Response
import RCL.Services
import RCL.Session



-- exported types

type RTMT m = StateT Session (ReaderT (Services m) (ErrorT Failure m))
type RTM    = RTMT Identity



-- exported functions

runRTM :: Monad m => Services m -> APIKey -> Secret -> RTMT m a -> m (Either Failure a)
runRTM c k s r = runErrorT $ flip runReaderT c $ evalStateT r $ Session k s "" ""

makeRTM :: Functor m => (Services m -> Session -> m (Either Failure a)) -> RTMT m a
makeRTM f = StateT (\s -> (,s) <$> ReaderT (\c -> ErrorT (f c s)))

runQuery :: Functor m => URL -> QueryBuilder -> RTMT m Response
runQuery u rb = makeRTM (\c s -> parseResponse <$> querant c (create s u rb))
