{-# LANGUAGE OverloadedStrings, BangPatterns #-}



import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Data.Maybe
import Data.List
import Network.URL
import System.Exit
import Test.HUnit
import Test.QuickCheck

import RCL.Constants
import RCL as R


session = Session "APIKEY" "SECRET" "TOKEN" "FROB"
services = Services answer
make = create session url
url = fromJust $ importURL "http://te.st"
https = "https" :: String
run = runIdentity . runRTM services "APIKEY" "SECRET"



answer !q
  | m "rtm.auth.checkToken" = Identity "{fail}"
  | m "rtm.auth.getFrob"    = Identity "{\"rsp\":{\"stat\":\"\",\"frob\":\"FROB2\"}}"
  | m "rtm.auth.getToken"   = Identity "{\"rsp\":{\"stat\":\"\",\"auth\":{\"token\":\"TOKEN2\"}}}"
  | otherwise               = Identity "{\"rsp\":{\"stat\":\"fail\",\"err\":{\"code\":\"42\",\"msg\":\"notfound\"}}}"
  where m x = x `isInfixOf` q





hTests = test [
  "Constants" ~: isJust (importURL restURLString) ~? "rest url should be valid",
  "Constants" ~: isJust (importURL authURLString) ~? "auth url should be valid",
  "Constants" ~: https `isPrefixOf` restURLString ~? "rest url should be secure",
  "Constants" ~: https `isPrefixOf` authURLString ~? "auth url should be secure",
  "Query"     ~: make sign ~?= "http://te.st/?api_key=APIKEY&api_sig=e7c078f57915f774c79daa5980719ece",
  "Auth"      ~: run (testToken "TOKEN") ~?= Right False,
  "Auth"      ~: run (getAuthURL  >> gets frob)  ~?= Right "FROB2",
  "Auth"      ~: run (getNewToken >> gets token) ~?= Right "TOKEN2",
  "E2E"       ~: run (R.get "missingMethod" [])  ~?= Left (R.Failure (42, "notfound"))
  ]


main = do
  hCount <- runTestTT hTests
  when (errors hCount + failures hCount > 0) exitFailure
