-- module

module RCL (module RCL, module RCLExport) where



-- imports

import Control.Monad
import Data.List

import RCL.Auth     as RCLExport
import RCL.Error    as RCLExport
import RCL.Query    as RCLExport
import RCL.Response as RCLExport
import RCL.RTM      as RCLExport
import RCL.Services as RCLExport
import RCL.Session  as RCLExport
import RCL.Types    as RCLExport

import RCL.Constants



-- exported functions

get :: (Monad m, Functor m) => String -> Parameters -> RTMT m Response
get m ps = runQuery restURL (method m >=> qp >=> auth >=> sign)
  where qp = foldl' (>=>) format $ map param ps
