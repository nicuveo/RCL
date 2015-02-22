{-# LANGUAGE TupleSections, OverloadedStrings #-}



-- module

module RCL.Auth (
  getAuthURL,
  getNewToken,
  setToken,
  testToken,
  ) where



-- imports

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Aeson

import RCL.Constants
import RCL.Error
import RCL.Query
import RCL.Response
import RCL.RTM
import RCL.Session



-- exported functions

testToken :: (Monad m, Functor m) => Token -> RTMT m Bool
testToken t = not <$> testError (getAuth q)
  where q = method "rtm.auth.checkToken" &: ("auth_token", t)

setToken :: Monad m => Token -> RTMT m ()
setToken t = modify (\s -> s { token = t })

getAuthURL :: (Functor m, Monad m) => RTMT m QueryURL
getAuthURL = do
  s <- get
  when (null $ frob s) getNewFrob
  makeAuthURL <$> get

getNewToken :: (Monad m, Functor m) => RTMT m Token
getNewToken = do
  s <- get
  a <- getAuth $ method "rtm.auth.getToken" >=> param ("frob", frob s)
  t <- extract ((.: "auth") >=> (.: "token")) a
  setToken t
  return t



-- internal functions

makeAuthURL :: Session -> QueryURL
makeAuthURL s = create s authURL $ param ("frob", frob s) &: ("perms", "read") >=> sign

getAuth :: Functor m => QueryBuilder -> RTMT m Response
getAuth = runQuery restURL . (>=> format >=> sign)

getNewFrob :: (Monad m, Functor m) => RTMT m ()
getNewFrob = do
  a <- getAuth $ method "rtm.auth.getFrob"
  f <- extract (.: "frob") a
  modify (\s -> s { frob = f })
