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

import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad
import Language.Haskell.TH
import qualified Data.Map as Map
import Data.Map (Map)
import Text.Printf

import Data.Binary hiding (get)

import Data.Typeable
import GHC.Generics (Generic)

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
          say $ printf "spawning on %s" (show nid)
          spawn nid $(mkStaticClosure 'datastore)
  mapM_ monitor ns


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
  -- For the exercise, change this to include your
  -- remote metadata, e.g. rcdata = Database.__remoteTable
