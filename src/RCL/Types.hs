-- module

module RCL.Types (
  APIKey,
  Frob,
  QueryURL,
  RawData,
  Secret,
  Timeline,
  Token,
  URL,
  ) where



-- imports

import           Data.ByteString.Lazy
import           Network.URL



-- exported types

type APIKey   = String
type Frob     = String
type QueryURL = String
type Secret   = String
type Timeline = String
type Token    = String

type RawData  = ByteString
