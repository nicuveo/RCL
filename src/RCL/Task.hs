{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}



-- module

module RCL.Task (
  Id,
  TaskId,
  SeriesId,
  ListId,
  Task(..),
  TaskSet,
  taskSetParser,
  taskSet,
  taskSetM,
  ) where



-- imports

import           Control.Monad
import           Control.Monad.Error
import           Data.Aeson
import           Data.Aeson.Types
import           Data.IxSet
import           Data.Text           (Text)
import           Data.Typeable
import           Data.Vector         as V hiding (empty, reverse)
import           Prelude             hiding (map)

import           RCL.Response



-- exported types

type Id = String

newtype TaskId   = TaskId   Id deriving (Show, Read, Eq, Ord, Typeable)
newtype SeriesId = SeriesId Id deriving (Show, Read, Eq, Ord, Typeable)
newtype ListId   = ListId   Id deriving (Show, Read, Eq, Ord, Typeable)

data Task = Task {
  taskId   :: TaskId,
  seriesId :: SeriesId,
  listId   :: ListId,
  name     :: Text
  } deriving (Show, Eq, Ord, Typeable)

type TaskSet = IxSet Task



-- instances

instance Indexable Task where
  empty = ixSet [ixFun ((:[]) . taskId)  ,
                 ixFun ((:[]) . seriesId),
                 ixFun ((:[]) . listId)  ]



-- exported functions

taskSetParser :: Response -> Parser TaskSet
taskSetParser r = do
  lists <- r .:* ["tasks", "list"]
  foldl1' unionTask $ map (parseList stub) lists
  where stub = Task undefined undefined undefined undefined

taskSet :: Response -> Either Failure TaskSet
taskSet = extract taskSetParser

taskSetM :: MonadError Failure m => Response -> m TaskSet
taskSetM = extractM taskSetParser



-- internal functions

(.:*) :: FromJSON a => Object -> [Text] -> Parser a
(.:*) o ps = combine (reverse ps) o
  where combine []     = undefined
        combine [x]    =              (.: x)
        combine (x:xs) = (.:* xs) >=> (.: x)


unionTask :: Parser TaskSet -> Parser TaskSet -> Parser TaskSet
unionTask = liftM2 union

insertTask :: Parser TaskSet -> Parser Task -> Parser TaskSet
insertTask = liftM2 $ flip insert


parseList   :: Task -> Value -> Parser TaskSet
parseList   stub = withObject "list object"   (parseListObject   stub)

parseSeries :: Task -> Value -> Parser TaskSet
parseSeries stub = withObject "series object" (parseSeriesObject stub)

parseTask   :: Task -> Value -> Parser Task
parseTask   stub = withObject "task object"   (parseTaskObject   stub)


parseListObject :: Task -> Object -> Parser TaskSet
parseListObject stub obj = do
  lid     <- obj .: "id"
  lseries <- obj .: "taskseries"
  let stub' = stub { listId = ListId lid }
  case lseries of
    Object o -> parseSeriesObject stub' o
    Array  a -> foldl1' unionTask $ map (parseSeries stub') a
    _        -> typeMismatch "series object or series array" lseries

parseSeriesObject :: Task -> Object -> Parser TaskSet
parseSeriesObject stub obj = do
  sid    <- obj .: "id"
  sname  <- obj .: "name"
  stasks <- obj .: "task"
  let stub' = stub { seriesId = SeriesId sid, name = sname }
      base  = return empty
  case stasks of
    Object o ->        insertTask base $ parseTaskObject stub'  o
    Array  a -> foldl' insertTask base $ map (parseTask  stub') a
    _        -> typeMismatch "task object or task array" stasks

parseTaskObject :: Task -> Object -> Parser Task
parseTaskObject stub obj = do
  tid <- obj .: "id"
  return $ stub { taskId = TaskId tid }
