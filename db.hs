{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
-}

module Database where

import Data.Map as Map
import Data.List
import System.IO 

--data type for database, maybe make this a lens?
data Database = Map.Map Key Value

type alias Key = String
type alias Value = String

initiateDB :: Process Database

set      :: Database -> Key -> Value -> Process ()

get      :: Database -> Key -> Process (Maybe Value)


main :: IO ()
main = do