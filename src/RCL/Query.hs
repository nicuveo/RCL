-- module

module RCL.Query (
  Parameter,
  Parameters,
  Query,
  QueryBuilder,
  QueryURL,
  URL,
  auth,
  create,
  format,
  method,
  param,
  sign,
  (&:)
  ) where



-- imports

import Control.Monad.Reader
import Data.Hash.MD5
import Data.List
import Network.URL

import RCL.Session



-- exported types

type Parameter    = (String, String)
type Parameters   = [Parameter]
type Query        = Reader Session Parameters
type QueryBuilder = Parameters -> Query
type QueryURL     = String



-- exported functions

param :: Parameter -> QueryBuilder
param p ps = return (p : ps)

method :: String -> QueryBuilder
method m = param ("method", m)

format :: QueryBuilder
format = param ("format", "json")

auth :: QueryBuilder
auth ps = reader $ \s -> ("auth_token", token s) : ps

sign :: QueryBuilder
sign ps = reader $ \s -> ("api_sig", md5s $ Str $ sig s) : ps
  where cat s (k, v) = s ++ k ++ v
        sig s = foldl' cat (secret s) $ sort ps

create :: Session -> URL -> QueryBuilder -> QueryURL
create s url qb = exportURL $ foldl' add_param url $ query qb s

(&:) :: QueryBuilder -> Parameter -> QueryBuilder
p &: q = p >=> param q



-- internal functions

query :: QueryBuilder -> Session -> Parameters
query qb s = runReader (qb [("api_key", apiKey s)]) s
