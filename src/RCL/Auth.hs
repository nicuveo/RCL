{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}



-- module

module RCL.Auth (
  getAuthURL,
  testToken,
  updateToken,
  ) where



-- imports

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Aeson
import           Data.Aeson.Types

import           RCL.Config
import           RCL.Error
import           RCL.Query
import           RCL.Response
import           RCL.RTM
import           RCL.Session
import           RCL.Types



-- exported functions

testToken :: (Monad m, Functor m) => Token -> RTMT m Bool
testToken t = not <$> testError (nonAuthCall q)
  where q = method "rtm.auth.checkToken" >=. ("auth_token", t)

getAuthURL :: (Functor m, Monad m) => RTMT m QueryURL
getAuthURL = do
  void getFrob
  c <- config
  s <- session
  return $ makeAuthURL c s

updateToken :: (Monad m, Functor m) => RTMT m Token
updateToken = do
  f <- getFrob
  a <- nonAuthCall $ method "rtm.auth.getToken" >=. ("frob", f)
  t <- extractM tokenParser a
  modify $ setToken t
  return t



-- internal functions

tokenParser :: Response -> Parser Token
tokenParser = (.: "auth") >=> (.: "token")

makeAuthURL :: Config -> Session -> QueryURL
makeAuthURL c s = create c s (authURL c) $ param ("frob", frob s) >=. ("perms", "delete") >=> sign

getFrob :: (Monad m, Functor m) => RTMT m Frob
getFrob = do
  f <- gets frob
  when (null f) $ do
    a <- nonAuthCall $ method "rtm.auth.getFrob"
    v <- extractM (.: "frob") a
    modify $ setFrob v
  gets frob
