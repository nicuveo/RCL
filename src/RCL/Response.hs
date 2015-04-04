{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeSynonymInstances       #-}



-- module

module RCL.Response (
  Failure (..),
  Response,
  extract,
  extractM,
  response,
  responseM,
  ) where



-- imports

import           Control.Arrow
import           Control.Monad.Error
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text
import           Text.Read           (readMaybe)

import           RCL.Error
import           RCL.Types



-- exported types

type Response = Object
newtype Failure = Failure (Int, String) deriving (Show, Eq, Ord)



-- instances

instance Error Failure where
  noMsg = jsonError jsonErrorMsg



-- exported functions

extract :: (a -> Parser b) -> a -> Either Failure b
extract p = left jsonError . parseEither p

response :: RawData -> Either Failure Response
response s = do
  jres <- left jsonError $ eitherDecode' s
  resp <- extract (.: "rsp" ) jres
  stat <- extract (.: "stat") resp
  case stat of
    String "fail" -> extractError resp
    _             -> Right        resp

extractM :: MonadError Failure m => (a -> Parser b) -> a -> m b
extractM p = withError . extract p

responseM :: MonadError Failure m => RawData -> m Response
responseM = withError . response




-- internal functions

jsonErrorCode :: Int
jsonErrorCode = -1

jsonErrorMsg :: String
jsonErrorMsg = "Unexpected JSON format"

jsonError :: String -> Failure
jsonError msg = Failure (jsonErrorCode, msg)

extractCode :: Value -> Parser Int
extractCode (String s) = maybe (fail "not a valid number") return $ readMaybe $ unpack s
extractCode j = parseJSON j

extractError :: Object -> Either Failure a
extractError = either Left Left . extract failure
  where failure rsp = do
          err  <- rsp .: "err"
          msg  <- err .: "msg"
          code <- err .: "code" >>= extractCode
          return $ Failure (code, msg)
