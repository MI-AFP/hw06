module MapTraveler (traveler) where

import Control.Monad.State

import Data.Geography

data Context = Context { ctMap :: Map
                       , ctPos :: Coords
                       }

walkTheMap :: StateT Context IO Coords
walkTheMap = undefined

doStep :: StateT Context IO Coords
doStep = undefined

traveler :: IO ()
traveler = putStrLn "The Map Traveler"
-- load map
-- randomly place traveler
-- iterate with state
