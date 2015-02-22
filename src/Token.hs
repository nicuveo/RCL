-- module

module Token (getToken) where



-- imports

import Codec.Binary.Base64.String
import Control.Applicative
import Control.Monad.Error
import System.Directory
import System.FilePath
import System.IO

import RCL



-- exported functions

getToken :: FilePath -> String -> RTMT IO Token
getToken path app = do
  file <- liftIO $ tokenFile path app
  tkv1 <- liftIO $ readToken file
  tkv2 <- validateToken tkv1
  tkv3 <- maybe createToken return tkv2
  liftIO $ writeToken file tkv3
  return tkv3



-- internal functions

tokenFile :: FilePath -> String -> IO FilePath
tokenFile path app = do
  dirName <- getAppUserDataDirectory app
  createDirectoryIfMissing True dirName
  return $ dirName </> path

readToken :: FilePath -> IO (Maybe Token)
readToken f = do
  tk <- runErrorT $ do
    fileExists <- liftIO $ doesFileExist f
    unless fileExists $ throwError ""
    fileReadable <- liftIO $ readable <$> getPermissions f
    unless fileReadable $ throwError ""
    decode <$> liftIO (readFile f)
  return $ either (const Nothing) Just tk

writeToken :: FilePath -> String -> IO ()
writeToken f = writeFile f . encode

validateToken :: Maybe Token -> RTMT IO (Maybe Token)
validateToken Nothing   = return Nothing
validateToken (Just tk) = do
  valid <- testToken tk
  return $ if valid
           then Just tk
           else Nothing

createToken :: RTMT IO Token
createToken = do
  authUrl <- getAuthURL
  liftIO $ do
    putStrLn "Couldn't find authentification token"
    putStrLn $ "Please authorize RCL: " ++ authUrl
    putStr "(Press enter when done.)"
    hFlush stdout
    void getLine
  getNewToken

