{-# LANGUAGE OverloadedStrings #-}



-- imports

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.IxSet                 hiding (null)
import           Data.List
import           Data.Maybe
import qualified Data.Text                  as T
import           Data.Text.Format
import qualified Data.Text.Lazy.IO          as L
import           Data.Time
import           Network.HTTP.Conduit       hiding (Proxy)
import           Network.URL
import           System.Console.ANSI
import           System.Console.GetOpt
import           System.Environment
import           System.Exit

import           Persistence
import           RCL                        hiding (format)



-- config

myRestURL  = fromJust $ importURL "https://api.rememberthemilk.com/services/rest"
myAuthURL  = fromJust $ importURL "https://api.rememberthemilk.com/services/auth"
myAPIKey   = "ed5366cee1c00e9fa3b036a54731ac2e"
mySecret   = "47b28ac806f42a22"

myConfig   = Config myRestURL myAuthURL myAPIKey mySecret

myFile     = "session.cfg"
myApp      = "rcl"



-- helpers

debugHttp x = do
  res <- simpleHttp x
  putStrLn $ "URL: " ++ x
  putStr "RESULT: "
  B.putStrLn res
  putStrLn ""
  return res


compareMaybe :: Ord a => Maybe a -> Maybe a -> Ordering
compareMaybe Nothing _ = GT
compareMaybe _ Nothing = LT
compareMaybe (Just t1) (Just t2) = compare t1 t2

printTask :: UTCTime -> Int -> Task -> IO ()
printTask d l t = do
  case compareMaybe (Just d) (dueOn t) of
    GT -> setSGR [SetColor Foreground Vivid Red]
    EQ -> setSGR [SetColor Foreground Vivid Yellow]
    LT -> setSGR [SetColor Foreground Dull  White]
  let n = right l ' ' $ name t
  let x = maybe "" show $ dueOn t
  L.putStrLn $ format "{}\t{}" (n, x)
  setSGR []

sortTasks :: [Task] -> [Task]
sortTasks = sortBy tc
  where tc t1 t2 = dueOn t1 `compareMaybe` dueOn t2

printError :: Failure -> IO ()
printError (Failure (c, e))
  | c < 0     = putStrLn $ "RCL error " ++ show (-c) ++ ": " ++ e
  | otherwise = putStrLn $ "RTM error " ++ show   c  ++ ": " ++ e

run :: Services IO -> RTMT IO a -> IO ()
run s r = do
  result <- runRTM myConfig s r
  case result of
   Left  e -> printError e
   Right _ -> return ()



-- options

data Flag = Debug deriving (Eq, Show)

options :: [OptDescr Flag]
options =
    [ Option "d" ["debug"] (NoArg Debug) "dump network IO on stderr"
    ]



-- main

main :: IO ()
main = do

  argv <- getArgs
  let (flags, _, errors) = getOpt Permute options argv
  unless (null errors) $ do
    mapM_ putStr errors
    exitFailure

  let s = Services $ if Debug `elem` flags
                     then debugHttp
                     else simpleHttp
  run s $ do
    loadSession myFile myApp
    saveSession myFile myApp
    tasks <- sortTasks . toAscList byName <$> getTasks incomplete
    ctime <- liftIO getCurrentTime
    let linel = maximum $ map (T.length . name) tasks
    liftIO $ mapM_ (printTask ctime linel) tasks
