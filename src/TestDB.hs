{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
-}

{-# OPTIONS_GHC -Wall #-}

module TestDB (main) where

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
--import Control.Monad.IO.Class
import Control.Monad
--import Data.List
--import System.IO
import System.Exit


import DistribUtils
import Database

main :: IO ()
main = distribMain masterTest rcdata


masterTest :: Backend -> [NodeId] -> Process ()
masterTest backend peers = do
	--build database
  db <- createDB peers

  --fill database with contents of
  --the Database.hs file
  f <- liftIO $ readFile "Database.hs"
  let ws = words f

  liftIO $ putStr "Populating Worker Processes...\n"
  zipWithM_ (set db) ws (tail ws)

  --test basic GET functionality
  liftIO $ putStr "GET test - should be 'Database':\n"
  get db "module" >>= liftIO . print

  liftIO $ putStr "GET test - should be 'Nothing':\n"
  get db "xxxx"   >>= liftIO . print

  --test basic SET functionailty
  liftIO $ putStr "SET test - new value, should be 'shark':\n"
  set db "xxxx" "shark"
  get db "xxxx"   >>= liftIO . print

  liftIO $ putStr "SET test - changed value, should be 'changed':\n"
  set db "module" "changed"
  get db "module" >>= liftIO . print

  --test functionality when new file data has been added
  liftIO $ putStr "ADDFILE test - added DistribUtils, GET 'defaultHost' should be '=':\n"
  file <- liftIO $ readFile "DistribUtils.hs"
  let wrds = words file
  zipWithM_ (set db) wrds (tail wrds)
  get db "defaultHost" >>= liftIO . print

  liftIO $ putStr "SET & GET of new file data - should be 'bestHost'\n"
  set db "defaultHost" "bestHost"
  get db "defaultHost" >>= liftIO . print

  --remove a process
  liftIO $ putStr "Purposefully causing a worker process to end\n"
  terminateSlave (head peers)

  --Test if previous data is still present through gets & sets
  --test new GET functionality
  liftIO $ putStr "GET test - should be 'DistribUtils':\n"
  get db "module" >>= liftIO . print

  liftIO $ putStr "GET test - should be 'shark':\n"
  get db "xxxx"   >>= liftIO . print

  --test new SET functionailty
  liftIO $ putStr "SET test - new value, should be 'barracuda':\n"
  set db "xxxx" "barracuda"
  get db "xxxx"   >>= liftIO . print

  liftIO $ putStr "SET test - changed value, should be '2changed':\n"
  set db "module" "2changed"
  get db "module" >>= liftIO . print

  --test that quitting successfully closes out the session
  liftIO $ putStr "QUIT test - closing and terminating remaining workers:\n"
  terminateAllSlaves backend
  _ <- liftIO $ exitSuccess
  return ()