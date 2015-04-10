{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}



-- module

module Persistence (loadSession, saveSession) where



-- imports

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.State
import           Data.Aeson                  as A
import           Data.Aeson.Types            as A
import qualified Data.ByteString.Base64.Lazy as B
import qualified Data.ByteString.Lazy        as B
import           System.Directory
import           System.FilePath
import           System.IO                   (hFlush, stdout)

import           RCL



-- exported functions

loadSession :: (Functor m, MonadIO m) => FilePath -> String -> RTMT m ()
loadSession path app = do
  file <- liftIO $ getFile path app
  readFrom file `catchError` const createNew
  where readFrom file = do
          guard =<< liftIO (doesFileExist file)
          guard =<< liftIO (readable <$> getPermissions file)
          rsess <-  liftIO (B.readFile file) >>= decodeSession
          guard =<< testToken (token rsess)
          put rsess

saveSession :: (Functor m, MonadIO m) => FilePath -> String -> RTMT m ()
saveSession path app = do
  file <- liftIO $ getFile path app
  sess <- encodeSession <$> session
  liftIO $ B.writeFile file sess



-- internal functions

createNew :: (Functor m, MonadIO m) => RTMT m ()
createNew = do
  authUrl <- getAuthURL
  liftIO $ do
    putStrLn "Couldn't find authentification token"
    putStrLn $ "Please authorize RCL: " ++ authUrl
    putStr "(Press enter when done.)"
    hFlush stdout
    void getLine
  void updateToken
  line <- call (method "rtm.timelines.create") >>= extractM (.: "timeline")
  modify $ setTimeline line

getFile :: FilePath -> String -> IO FilePath
getFile path app = do
  dirName <- getAppUserDataDirectory app
  createDirectoryIfMissing True dirName
  return $ dirName </> path

decodeSession :: MonadError Failure m => B.ByteString -> m Session
decodeSession = either (throwError . strMsg) return . (B.decode >=> A.eitherDecode >=> parseJ)

encodeSession :: Session -> B.ByteString
encodeSession = B.encode . A.encode . toJ

parseJ :: Value -> Either String Session
parseJ = parseEither $ withObject "session object" $ \o -> do
  tk <- o .: "token"
  tl <- o .: "timeline"
  return $ Session tl tk ""

toJ :: Session -> Value
toJ s = object [
  "timeline" .= timeline s,
  "token"    .= token s
  ]
