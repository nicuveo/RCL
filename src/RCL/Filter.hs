{-# LANGUAGE TupleSections #-}



-- module

module RCL.Filter (
  Status(..),
  Filter(..),
  filterWith,
  completed,
  incomplete,
  noPriority,
  hasPriority,
  priorityIn,
  repeated,
  oneshot,
  isTagged,
  hasNoTag,
  withNotes,
  withoutNotes,
  andF,
  orF,
  ) where



-- imports

import           Data.List (foldl1')

import           RCL.Query
import           RCL.Task



-- exported types

data Filter = Expr            String
            | Name            String
            | List            String
            | Status          Status
            | Priority        Priority
            | Location        String
            | TagIs           String
            | TagContains     String
            | NoteContains    String
            | DueOn           String
            | DueAfter        String
            | DueBefore       String
            | DueWithin       String
            | AddedOn         String
            | AddedAfter      String
            | AddedBefore     String
            | AddedWithin     String
            | CompletedOn     String
            | CompletedAfter  String
            | CompletedBefore String
            | CompletedWithin String
            | IsRepeating     Bool
            | IsTagged        Bool
            | HasNotes        Bool
            | Not             Filter
            | (:&&:)          Filter Filter
            | (:||:)          Filter Filter

infixr 3 :&&:
infixr 2 :||:



-- instances

instance Show Filter where
  show (Expr s)            = s
  show (Name s)            = "name:"           ++ s
  show (List s)            = "list:"           ++ s
  show (Status s)          = "status:"         ++ show s
  show (Priority p)        = "priority:"       ++ show p
  show (Location s)        = "location:"       ++ s
  show (TagIs s)           = "tag:"            ++ s
  show (TagContains s)     = "tagContains:"    ++ s
  show (NoteContains s)    = "noteContains:"   ++ s
  show (DueOn s)           = "due"             ++ s
  show (DueAfter s)        = "dueAfter"        ++ s
  show (DueBefore s)       = "dueBefore"       ++ s
  show (DueWithin s)       = "dueWithin"       ++ s
  show (AddedOn s)         = "addes"           ++ s
  show (AddedAfter s)      = "addedAfter"      ++ s
  show (AddedBefore s)     = "addedBefore"     ++ s
  show (AddedWithin s)     = "addedWithin"     ++ s
  show (CompletedOn s)     = "completed"       ++ s
  show (CompletedAfter s)  = "completedAfter"  ++ s
  show (CompletedBefore s) = "completedBefore" ++ s
  show (CompletedWithin s) = "completedWithin" ++ s
  show (IsRepeating b)     = "isRepeating:"    ++ show b
  show (IsTagged b)        = "isTagged:"       ++ show b
  show (HasNotes b)        = "hasNotes:"       ++ show b
  show (Not f)             = "NOT (" ++ show f ++ ")"
  show (f1 :&&: f2)        = "(" ++ show f1 ++ ") AND (" ++ show f2 ++ ")"
  show (f1 :||: f2)        = "(" ++ show f1 ++ ") OR ("  ++ show f2 ++ ")"



-- exported functions

filterWith :: Filter -> Parameter
filterWith = ("filter",) . show

completed, incomplete, noPriority, hasPriority, repeated, oneshot :: Filter
isTagged, hasNoTag, withNotes, withoutNotes                       :: Filter
completed    = Status Completed
incomplete   = Status Incomplete
noPriority   = Priority None
hasPriority  = Not noPriority
repeated     = IsRepeating True
oneshot      = IsRepeating False
isTagged     = IsTagged    True
hasNoTag     = IsTagged    False
withNotes    = HasNotes    True
withoutNotes = HasNotes    False

priorityIn :: [Priority] -> Filter
priorityIn = orF . map Priority

andF, orF :: [Filter] -> Filter
andF = foldl1' (:&&:)
orF  = foldl1' (:&&:)
