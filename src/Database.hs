{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
-}

module Database (
       Database,
       Key, Value,
       createDB,
       get, set,
       rcdata,
  ) where

import Control.Distributed.Process

import qualified Data.Map as Map
import Data.Map (Map)

type Key   = String
type Value = String

type Database = ProcessId

createDB :: [NodeId] -> Process Database
createDB nodes = case nodes of
	[] -> 
	n:ns -> spawn n $(mkStaticClosure)


set :: Database -> Key -> Value -> Process ()
set db k v = error "not implemented!" -- exercise

get :: Database -> Key -> Process (Maybe Value)
get db k = error "not implemented!" -- exercise

rcdata :: RemoteTable -> RemoteTable
rcdata = id
  -- For the exercise, change this to include your
  -- remote metadata, e.g. rcdata = Database.__remoteTable