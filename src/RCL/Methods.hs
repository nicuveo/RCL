{-# LANGUAGE OverloadedStrings #-}



-- module

module RCL.Methods where



-- imports

import           RCL.Filter
import           RCL.Query
import           RCL.RTM
import           RCL.Task



-- exported functions

getTasks :: (Monad m, Functor m) => Filter -> RTMT m TaskSet
getTasks f = taskSetM =<< call (method "rtm.tasks.getList" >=. filterWith f)

getAllTasks :: (Monad m, Functor m) => RTMT m TaskSet
getAllTasks = taskSetM =<< call (method "rtm.tasks.getList")
