{-# LANGUAGE OverloadedStrings #-}



-- module

module RCL.Constants where



-- imports

import Data.Maybe
import Data.String
import Network.URL



-- exported functions

restURLString :: IsString s => s
restURLString = "https://api.rememberthemilk.com/services/rest"

authURLString :: IsString s => s
authURLString = "https://api.rememberthemilk.com/services/auth"

restURL :: URL
restURL = fromJust $ importURL restURLString

authURL :: URL
authURL = fromJust $ importURL authURLString
