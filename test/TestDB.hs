{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
-}

module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Exception
import System.Environment (setEnv, getEnv)
import System.IO
import Control.Concurrent (forkFinally)
import Text.Printf (printf)
import Control.Concurrent.STM

import Master
import DistribUtils


main = do
--get db "module" >>= liftIO . print
--  get db "xxxx"   >>= liftIO . print