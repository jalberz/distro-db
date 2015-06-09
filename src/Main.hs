{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
-}

import Master
import Database
import DistribUtils

--Run our distributed key-value store
main :: IO ()
main = distribMain master rcdata