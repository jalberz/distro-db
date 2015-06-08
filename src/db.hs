{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
-}

{-# LANGUAGE OverloadedStrings #-}

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad.IO.Class
import Control.Monad
import System.IO
import System.Exit

import DistribUtils

import Database  (Database, createDB, get, set, rcdata)

main = distribMain master rcdata

master :: Backend -> [NodeId] -> Process ()
master backend peers = do
  db <- createDB peers

  --liftIO $ putStr "file: "
  --filename <- liftIO $ getLine
  f <- liftIO $ readFile "Database.hs"
  let ws = words f


  liftIO $ putStr "Populating Worker Processes...\n"
  zipWithM_ (set db) ws (tail ws)

  liftIO $ putStr "Commands: GET <key>, SET <key> <value>, KILL <NodeId>, QUIT\n"

  get db "module" >>= liftIO . print
  get db "xxxx"   >>= liftIO . print

  forever $ do
    l <- liftIO $ do putStr "> "; hFlush stdout; getLine
    when (not (null l)) $ do
      case (words l) of
        ["GET", k] -> do
          r <- get db k
          liftIO $ putStrLn ("response: " ++ show r)
        ["SET", k, v] -> do
          set db  k v
          liftIO $ putStrLn ("response: " ++ show k ++ "set to" ++ show v)
        --["KILL", n ] -> do
        --  terminateSlave (read n :: NodeId)
        ["QUIT"] -> do
          terminateAllSlaves backend
          liftIO $ exitSuccess
        _ -> do
          liftIO $ putStrLn ("command not recognized")

  terminateAllSlaves backend
  return ()