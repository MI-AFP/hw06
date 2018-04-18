{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Geography where

import Control.Lens

import Data.QuadTree

data Coords = Coords { _x :: Integer, _y :: Integer }
            deriving (Show, Read, Eq)

makeLenses ''Coords

data Direction = North | South | West | East
               deriving (Show, Read, Eq, Enum, Bounded)

data MapField = Grass | Road | Desert | Water | Obstacle | Tree
              deriving (Show, Read, Eq, Enum, Bounded)

type Map = QuadTree MapField

walkable :: MapField -> Bool
walkable Grass = True
walkable Desert = True
walkable Road = True
walkable _ = False

displayMap :: Map -> (MapField -> Char) -> [String]
displayMap = undefined

newCoords :: Coords -> Direction -> Coords
newCoords = undefined

getDirection :: Coords -> Coords -> Maybe Direction
getDirection = undefined
