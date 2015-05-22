{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
-}

{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
--{-# OPTIONS_GHC -Wall #-}

{-}
module Database (
       Database,
       Key, Value,
       createDB,
       get, set,
       rcdata,
  ) where
-}
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Language.Haskell.TH
import qualified Data.Map as Map
import Data.Map (Map)
import Text.Printf

import Data.Binary

import Data.Typeable
import GHC.Generics (Generic)

type Key   = String
type Value = String

type Database = ProcessId

--Message type
data Message = Set ProcessId
              | Get ProcessId
    deriving (Typeable, Generic)

instance Binary Message

--database process
database :: Process ()
database = do
  Set from <- expect
  say $ printf "set received from %s" (show from)
  Get from <- expect
  say $ printf "get received from %s" (show from)
  --mypid <- getSelfPid -- for future communication between nodes

--remotable call for database
remotable ['database]

createDB :: [NodeId] -> Process Database
createDB nodes = case nodes of
	[] -> error "empty nodeid"
	n:ns -> spawn n $(mkStaticClosure 'database)


set :: Database -> Key -> Value -> Process ()
set db k v = error "not implemented!" -- exercise

get :: Database -> Key -> Process (Maybe Value)
get db k = error "not implemented!" -- exercise

rcdata :: RemoteTable -> RemoteTable
rcdata = id
  -- For the exercise, change this to include your
  -- remote metadata, e.g. rcdata = Database.__remoteTable