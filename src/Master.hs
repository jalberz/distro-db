{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
-}

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

--Code for building the Master Process
module Master where

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad
import System.IO
import System.Exit

import Database (createDB, get, set)

{-
Following function runs the master process, which runs the various
commands from the terminal to the worker processes and runs their
responses back.
-}
master :: Backend -> [NodeId] -> Process ()
master backend peers = do
  --build datastore
  db <- createDB peers

  liftIO $ putStr "Commands: ADDFILE <file>, GET <key>, SET <key> <value>, KILL <NodeId>, QUIT\n"
  --warm up db
  get db "module" >>= liftIO . print
  get db "xxxx"   >>= liftIO . print
  _ <- forever $ do
    l <- liftIO $ do putStr "> "; hFlush stdout; getLine
    when (not (null l)) $ do
      case (words l) of
        ["ADDFILE", name] -> do
          --read file and move it's contents to datastore
          file <- liftIO $ readFile name
          let wrds = words file
          zipWithM_ (set db) wrds (tail wrds)
          liftIO $ putStrLn ("Worker processes populated with new file data")
        ["GET", k] -> do
          r <- get db k
          liftIO $ putStrLn ("response: " ++ show r)
        ["SET", k, v] -> do
          set db  k v
          liftIO $ putStrLn ("response: " ++ show k ++ "set to" ++ show v)
        ["QUIT"] -> do
          terminateAllSlaves backend
          liftIO $ exitSuccess
        _ -> do
          liftIO $ putStrLn ("command not recognized")

  terminateAllSlaves backend
  return ()