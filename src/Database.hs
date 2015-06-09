{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
-}

{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}


module Database (
       Database,
       Key, Value,
       createDB,
       get, set,
       rcdata,
  ) where

--Distribtued Process pacakges
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure

--Control Packages
import Control.Exception hiding (catch)
import Control.Monad (forM, when)

--Data Packages
import Data.Binary hiding (get)
import Data.Char
import qualified Data.Map as Map
import Data.Typeable

--Other Packages
import GHC.Generics (Generic)
import Prelude
import Text.Printf (printf)

--Basic Values for Keys and Values
type Key   = String
type Value = String

type Database = ProcessId

--Message type
data Message = Set Key Value
              | Get Key (SendPort (Maybe Value))
    deriving (Typeable, Generic)

instance Binary Message

{-
running the database process through 
updating a running Map
-}
datastore :: Process ()
datastore = runDB Map.empty where
  runDB store = do
    msg <- expect
    case msg of
      Set k v -> runDB (Map.insert k v store)
      Get k sp -> do
        sendChan sp (Map.lookup k store)
        runDB store

--remotable call for database
remotable ['datastore]

--given node id, generate a new database process
createDB :: [NodeId] -> Process Database
createDB nodes = spawnLocal $ do
  ns <- forM nodes $ \nid -> do
          say $ printf "spawning process on %s" (show nid)
          spawn nid $(mkStaticClosure 'datastore)
  when (null ns) $ liftIO $ ioError (userError "worker list is empty!")
  mapM_ monitor ns
  let 
    workerGroups = groupWorkers ns
    partitions = length workerGroups
  processMessages workerGroups partitions

--Group workers by pairs, overlapping them
groupWorkers :: [ProcessId] -> [[ProcessId]]
groupWorkers pids = case pids of
  [] -> []
  (a:b:cs) -> [a,b]:(groupWorkers cs)
  (_:[]) -> []

{-
The receiveWait function waits untils one of the match options in the list of
possible outcomes becomes true, then it executes that particular action
-}
processMessages :: [[ProcessId]] -> Int -> Process ()
processMessages workerGroups partitions
 = receiveWait
        [ match $ \msg -> (handleMsg msg workerGroups partitions)
          >> processMessages workerGroups partitions
        , match $ \(ProcessMonitorNotification _ pid reason) -> do
            say (printf "process %s died: %s" (show pid) (show reason))
            processMessages (map (filter (/= pid)) workerGroups) partitions
        ]

{-
Handle a message by dispersing it out to all the processes with that
key in their key space
-}
handleMsg :: Message -> [[ProcessId]] -> Int -> Process ()
handleMsg m workerGroups partitions = case m of
    Get k _ -> disperseMsg k m workerGroups partitions
    Set k _ -> disperseMsg k m workerGroups partitions

{-
disperse the messages amongst the worker processes
-}    
disperseMsg :: Key -> Message -> [[ProcessId]] -> Int -> Process ()
disperseMsg k m workerGroups partitions = mapM_ (\x -> send x m) (keyWorkers k) where
  keyWorkers key = workerGroups !! (index key)
  index key = ord (head key) `mod` partitions

{-
send a set message, establishing that
a given key, which may or may no already be in the 
datastore, be coupled with a new value
-}
set :: Database -> Key -> Value -> Process ()
set db k v = do
  send db (Set k v)

{-
get the value associated with a specific key from the datastore
-}
get :: Database -> Key -> Process (Maybe Value)
get db k = do
  (sp, rp) <- newChan
  send db (Get k sp)
  receiveChan rp

{-
this establishes which remotetable will
be used by the datastore (see distribmain)
-}
rcdata :: RemoteTable -> RemoteTable
rcdata = Database.__remoteTable
