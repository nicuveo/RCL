{-# LANGUAGE OverloadedStrings #-}



-- imports

import           Control.Monad.IO.Class
import           Data.IxSet
import           Data.Text              as T hiding (map)
import           Data.Text.IO           as T hiding (putStrLn, hPutStrLn)
import           Network.HTTP.Conduit

import           RCL
import           Token



-- helpers

myAPIKey = "ed5366cee1c00e9fa3b036a54731ac2e"
mySecret = "47b28ac806f42a22"
myFile   = "token.cfg"
myApp    = "rcl"

services :: Services IO
services = Services {
  querant = simpleHttp
  }



-- main

printError :: Failure -> IO ()
printError (Failure (c, e))
  | c < 0     = putStrLn $ "RCL error " ++ show (-c) ++ ": " ++ e
  | otherwise = putStrLn $ "RTM error " ++ show   c  ++ ": " ++ e

run :: RTMT IO a -> IO ()
run r = do
  result <- runRTM services myAPIKey mySecret r
  case result of
   Left  e -> printError e
   Right _ -> return ()

main :: IO ()
main = run $ do
  getToken myFile myApp >>= setToken
  resp <- get "rtm.tasks.getList" [("filter", "status:incomplete")]
  tset <- taskSetM resp
  liftIO $ T.putStr $ T.unlines $ map name $ toList tset
