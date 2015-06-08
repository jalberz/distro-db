{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
-}

{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
--{-# OPTIONS_GHC -Wall #-}


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
import Control.Concurrent hiding (newChan)
import Control.Exception hiding (catch)
import Control.Monad
import Control.Monad.IO.Class

--Data Packages
import qualified Data.Binary
import Data.Binary hiding (get)
import Data.Char
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Typeable

--Other Packages
import GHC.Generics (Generic)
import Language.Haskell.TH hiding (match)
import Prelude hiding (catch)
import System.IO.Error hiding (catch)
import Text.Printf


--Basic Values for Keys and Values
type Key   = String
type Value = String

type Database = ProcessId

--Message type
data Message = Set Key Value
              | Get Key (SendPort (Maybe Value))
    deriving (Typeable, Generic)

instance Binary Message

--database process
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
  when (null ns) $ liftIO $ ioError (userError "no workers")
  mapM_ monitor ns
  let 
    workerGroups = groupWorkers ns
    nSlices = length workerGroups
  messageLoop workerGroups nSlices

--Group workers by pairs TODO: change this
groupWorkers :: [ProcessId] -> [[ProcessId]]
groupWorkers [] = []
groupWorkers (a:b:xs) = [a,b] : groupWorkers xs
groupWorkers [x] = []

{-
The receiveWait function waits untils one of the match options in the list of
possible outcomes becomes true, then it executes that particular action
-}
messageLoop :: [[ProcessId]] -> Int -> Process ()
messageLoop workerGroups nSlices
 = receiveWait
        [ match $ \msg -> handleRequest msg workerGroups nSlices
          >> messageLoop workerGroups nSlices
        , match $ \(ProcessMonitorNotification _ pid reason) -> do
            say (printf "process %s died: %s" (show pid) (show reason))
            messageLoop (map (filter (/= pid)) workerGroups) nSlices
        ]

{-
Handle a request by dispersing it out to all the processes with that
key in their key space
-}
handleRequest :: Message -> [[ProcessId]] -> Int -> Process ()
handleRequest m workerGroups nSlices =
  case m of
    Set k _ -> mapM_ (\x -> send x m) (keyWorkers k)
    Get k _ -> mapM_ (\x -> send x m) (keyWorkers k)
  where
    keyWorkers :: Key -> [ProcessId]
    keyWorkers k = workerGroups !! (ord (head k) `mod` nSlices)

set :: Database -> Key -> Value -> Process ()
set db k v = do
  send db (Set k v)


get :: Database -> Key -> Process (Maybe Value)
get db k = do
  (sp, rp) <- newChan
  send db (Get k sp)
  receiveChan rp

rcdata :: RemoteTable -> RemoteTable
rcdata = Database.__remoteTable
