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
  Status(..),
  Priority(..),
  Task(..),
  TaskSet,
  taskSetParser,
  taskSet,
  taskSetM,
  byListId,
  bySeriesId,
  byTaskId,
  byName,
  byDueDate,
  byAddedDate,
  byDeletedDate,
  byCompletedDate,
  ) where



-- imports

import           Control.Applicative hiding (empty)
import           Control.Monad
import           Control.Monad.Error
import           Data.Aeson
import           Data.Aeson.Types
import           Data.IxSet
import           Data.Maybe
import           Data.Text           (Text)
import           Data.Time           as T
import           Data.Time.ISO8601
import           Data.Typeable       hiding (Proxy)
import           Data.Vector         as V hiding (empty, reverse, (++))
import           Prelude             hiding (map)

import           RCL.Response



-- exported types

type Id = String

newtype TaskId        = TaskId        Id      deriving (Show, Read, Eq, Ord, Typeable)
newtype SeriesId      = SeriesId      Id      deriving (Show, Read, Eq, Ord, Typeable)
newtype ListId        = ListId        Id      deriving (Show, Read, Eq, Ord, Typeable)
newtype DueDate       = DueDate       UTCTime deriving (Show, Read, Eq, Ord, Typeable)
newtype AddedDate     = AddedDate     UTCTime deriving (Show, Read, Eq, Ord, Typeable)
newtype DeletedDate   = DeletedDate   UTCTime deriving (Show, Read, Eq, Ord, Typeable)
newtype CompletedDate = CompletedDate UTCTime deriving (Show, Read, Eq, Ord, Typeable)

data Status   = Completed | Incomplete deriving (Eq, Ord)
data Priority = None | P1 | P2 | P3    deriving (Eq, Ord)

data Task = Task {
  taskId      :: TaskId,
  seriesId    :: SeriesId,
  listId      :: ListId,
  name        :: Text,
  dueTime     :: Bool,
  dueOn       :: Maybe UTCTime,
  addedOn     :: Maybe UTCTime,
  completedOn :: Maybe UTCTime,
  deletedOn   :: Maybe UTCTime
  } deriving (Show, Typeable)

type TaskSet = IxSet Task



-- instances

instance Show Status where
  show Completed  = "completed"
  show Incomplete = "incomplete"

instance Show Priority where
  show None = "none"
  show P1   = "1"
  show P2   = "2"
  show P3   = "3"

instance Read Priority where
  readsPrec _ ('N':s) = [(None, s)]
  readsPrec _ ('1':s) = [(P1,   s)]
  readsPrec _ ('2':s) = [(P2,   s)]
  readsPrec _ ('3':s) = [(P3,   s)]
  readsPrec _ s    = error $ "read Priority: unexpected value '" ++ s ++ "'"

instance Indexable Task where
  empty = ixSet [ ixFun (pure . taskId)
                , ixFun (pure . seriesId)
                , ixFun (pure . listId)
                , ixFun (pure . name)
                , ixFun (maybeToList . fmap DueDate . dueOn)
                , ixFun (maybeToList . fmap AddedDate . addedOn)
                , ixFun (maybeToList . fmap DeletedDate . deletedOn)
                , ixFun (maybeToList . fmap CompletedDate . completedOn)
                ]

instance Ord Task where
  compare t1 t2 = compare (taskId t1) (taskId t2)

instance Eq Task where
  t1 == t2 = taskId t1 == taskId t2



-- exported functions

taskSetParser :: Response -> Parser TaskSet
taskSetParser r = do
  lists <- r .:* ["tasks", "list"]
  foldl1' unionTask $ map (parseList stub) lists
  where stub = Task u u u u u u u u u
        u    = undefined

taskSet :: Response -> Either Failure TaskSet
taskSet = extract taskSetParser

taskSetM :: MonadError Failure m => Response -> m TaskSet
taskSetM = extractM taskSetParser


byListId :: Proxy ListId
byListId = Proxy

bySeriesId :: Proxy SeriesId
bySeriesId = Proxy

byTaskId :: Proxy TaskId
byTaskId = Proxy

byName :: Proxy Text
byName = Proxy

byDueDate :: Proxy DueDate
byDueDate = Proxy

byAddedDate :: Proxy AddedDate
byAddedDate = Proxy

byDeletedDate :: Proxy DeletedDate
byDeletedDate = Proxy

byCompletedDate :: Proxy CompletedDate
byCompletedDate = Proxy



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
  duet <- obj .:? "has_due_time" .!= ("0" :: String)
  tadd <- obj .:? "added"
  tcmp <- obj .:? "completed"
  tdel <- obj .:? "deleted"
  tdue <- obj .:? "due"
  tid  <- obj .:  "id"
  return $ stub {
    addedOn     = join $ parseISO8601 <$> tadd,
    completedOn = join $ parseISO8601 <$> tcmp,
    deletedOn   = join $ parseISO8601 <$> tdel,
    dueOn       = join $ parseISO8601 <$> tdue,
    dueTime     = duet /= "0",
    taskId      = TaskId tid
    }
