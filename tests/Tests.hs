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

import RCL as R

testRestURL  = fromJust $ importURL "https://api.com/rest"
testAuthURL  = fromJust $ importURL "https://api.com/auth"
testConfig   = Config testRestURL testAuthURL "APIKEY" "SECRET"
testSession  = Session "TIMELINE" "TOKEN" "FROB"
testServices = Services answer

make = create testConfig testSession testRestURL
run  = runIdentity . runRTM testConfig testServices



answer !q
  | m "rtm.auth.checkToken" = Identity "{fail}"
  | m "rtm.auth.getFrob"    = Identity "{\"rsp\":{\"stat\":\"\",\"frob\":\"FROB2\"}}"
  | m "rtm.auth.getToken"   = Identity "{\"rsp\":{\"stat\":\"\",\"auth\":{\"token\":\"TOKEN2\"}}}"
  | otherwise               = Identity "{\"rsp\":{\"stat\":\"fail\",\"err\":{\"code\":\"42\",\"msg\":\"notfound\"}}}"
  where m x = x `isInfixOf` q





hTests = test [
  "Query" ~: make sign ~?= "https://api.com/rest?api_key=APIKEY&api_sig=e7c078f57915f774c79daa5980719ece",
  "Auth"  ~: run (testToken "TOKEN") ~?= Right False,
  "Auth"  ~: run (getAuthURL  >> gets frob)  ~?= Right "FROB2",
  "Auth"  ~: run (updateToken >> gets token) ~?= Right "TOKEN2",
  "E2E"   ~: run (call $ method "missingMethod") ~?= Left (R.Failure (42, "notfound"))
  ]


main = do
  hCount <- runTestTT hTests
  when (errors hCount + failures hCount > 0) exitFailure
