{-# LANGUAGE
    FlexibleContexts,
    FlexibleInstances,
    GeneralizedNewtypeDeriving,
    OverloadedStrings,
    TupleSections,
    TypeSynonymInstances #-}



-- module

module RCL.Response (
  Failure (..),
  Response,
  extract,
  parseResponse,
  ) where



-- imports

import Control.Arrow
import Control.Monad.Error
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Text.Read (readMaybe)

import RCL.Error
import RCL.Types



-- exported types

type Response = Object
newtype Failure = Failure (Int, String) deriving (Show, Eq, Ord)



-- instances

instance Error Failure where
  noMsg = jsonError



-- exported functions

parseResponse :: RawData -> Either Failure Response
parseResponse s = do
  jres <- maybe (Left jsonError) Right $ decode s
  resp <- parseError (.:  "rsp") jres
  stat <- parseError (.: "stat") resp
  case stat of
   String "fail" -> either Left Left $ parseError extractError resp
   _             -> Right resp

extract :: MonadError Failure m => (a -> Parser b) -> a -> m b
extract p r = withError $ parseError p r


-- internal functions

jsonErrorCode :: Int
jsonErrorCode = -1

jsonErrorMsg :: String
jsonErrorMsg = "Unexpected JSON format"

jsonError :: Failure
jsonError = Failure (jsonErrorCode, jsonErrorMsg)

parseError :: (a -> Parser b) -> a -> Either Failure b
parseError p = left (Failure . (jsonErrorCode,)) . parseEither p

extractCode :: Value -> Parser Int
extractCode (String s) = maybe (fail "not a valid number") return $ readMaybe $ unpack s
extractCode j = parseJSON j

extractError :: Object -> Parser Failure
extractError rsp = do
  err  <- rsp .: "err"
  msg  <- err .: "msg"
  code <- err .: "code" >>= extractCode
  return $ Failure (code, msg)
