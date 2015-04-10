-- module

module RCL.Query (
  Parameter,
  Parameters,
  Query,
  QueryBuilder,
  auth,
  create,
  format,
  method,
  param,
  params,
  sign,
  tline,
  (>=.)
  ) where



-- imports

import           Control.Monad.Reader
import           Data.Hash.MD5
import           Data.List
import           Network.URL

import           RCL.Config
import           RCL.Session
import           RCL.Types



-- exported types

type Parameter    = (String, String)
type Parameters   = [Parameter]
type Context      = (Config, Session)
type Query        = Reader Context Parameters
type QueryBuilder = Parameters -> Query



-- exported functions

param :: Parameter -> QueryBuilder
param p ps = return $ p : ps

params :: Parameters -> QueryBuilder
params ps1 ps2 = return $ ps1 ++ ps2

method :: String -> QueryBuilder
method m = param ("method", m)

format :: QueryBuilder
format = param ("format", "json")

auth :: QueryBuilder
auth ps = reader $ \(_, s) -> ("auth_token", token s) : ps

tline :: QueryBuilder
tline ps = reader $ \(_, s) -> ("timeline", timeline s) : ps

sign :: QueryBuilder
sign ps = reader $ \(c, _) -> ("api_sig", md5s $ Str $ sig c) : ps
  where cat s (k, v) = s ++ k ++ v
        sig c = foldl' cat (secret c) $ sort ps

create :: Config -> Session -> URL -> QueryBuilder -> QueryURL
create c s url qb = exportURL $ foldl' add_param url $ makeQuery qb (c, s)

(>=.) :: QueryBuilder -> Parameter -> QueryBuilder
p >=. q = p >=> param q



-- internal functions

makeQuery :: QueryBuilder -> Context -> Parameters
makeQuery qb p@(c, _) = runReader (qb [("api_key", apiKey c)]) p
