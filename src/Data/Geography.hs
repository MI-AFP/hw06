{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Geography where

import Control.Lens

import Data.QuadTree

-- | Coordinates data structure
data Coords = Coords { _x :: Integer  -- ^ X (horizontal) coordinate
                     , _y :: Integer  -- ^ Y (vertical) coordinate
                     }
            deriving (Show, Read, Eq)

makeLenses ''Coords

-- | Enum for 4 basic directions
data Direction = North | South | West | East
               deriving (Show, Read, Eq, Enum, Bounded)

-- | Enum for map field types
data MapField = Grass | Road | Desert | Water | Obstacle | Tree
              deriving (Show, Read, Eq, Enum, Bounded)

-- | Map is 'QuadTree' of 'MapField's
type Map = QuadTree MapField

-- | Tell if given 'MapField' is walkable
walkable :: MapField -> Bool
walkable Grass = True
walkable Desert = True
walkable Road = True
walkable _ = False

-- | Convert given 'Map' to 'Char' matrix by given transformation
--
-- TODO: implement
displayMap :: Map -> (MapField -> Char) -> [String]
displayMap = undefined

-- | Convert given 'Char' matrix to 'Map' by given transformation
--
-- TODO: implement
loadMap :: [String] -> (Char -> MapField) -> Map
loadMap = undefined

-- | Update coordinates by moving one step with given direction
--
-- TODO: implement
newCoords :: Coords -> Direction -> Coords
newCoords = undefined

-- | Get direction from two coordinates ('Nothing' if the same)
--
-- TODO: implement
getDirection :: Coords -> Coords -> Maybe Direction
getDirection = undefined
