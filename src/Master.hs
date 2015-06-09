{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
-}

{-# LANGUAGE OverloadedStrings #-}

--Code for building the Master Process
module Master where

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad.IO.Class
import Control.Monad
import Data.List
import System.IO
import System.Exit

import DistribUtils

import Database  (Database, createDB, get, set, rcdata)

master :: Backend -> [NodeId] -> Process ()
master backend peers = do
  db <- createDB peers

  liftIO $ putStr "Commands: ADDFILE <file>, GET <key>, SET <key> <value>, KILL <NodeId>, QUIT\n"
  get db "module" >>= liftIO . print
  get db "xxxx"   >>= liftIO . print
  forever $ do
    l <- liftIO $ do putStr "> "; hFlush stdout; getLine
    when (not (null l)) $ do
      case (words l) of
        ["ADDFILE", name] -> do
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