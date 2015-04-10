{-# LANGUAGE OverloadedStrings #-}



-- module

module RCL.Session (
  Session(..),
  setTimeline,
  setToken,
  setFrob,
  emptySession,
  ) where



-- import

import           RCL.Types



-- exported types

data Session = Session {
  timeline :: Timeline,
  token    :: Token,
  frob     :: Frob
  }



-- exported functions

setTimeline :: Timeline -> Session -> Session
setToken    :: Token    -> Session -> Session
setFrob     :: Frob     -> Session -> Session

setTimeline v s = s { timeline = v }
setToken    v s = s { token    = v }
setFrob     v s = s { frob     = v }


emptySession :: Session
emptySession = Session "" "" ""
