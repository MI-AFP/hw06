module Data.Geography.Styles where

import Data.Geography

import Data.Map as M

defaultChar = '?'

simpleChars :: [(MapField, Char)]
simpleChars = [ (Grass, 'G')
              , (Road, 'R')
              , (Desert, 'D')
              , (Water, 'W')
              , (Obstacle, 'O')
              , (Tree, 'T')
              ]

ascii :: [(MapField, Char)]
ascii = [ (Grass, ' ')
        , (Road, '+')
        , (Desert, '*')
        , (Water, '~')
        , (Obstacle, '#')
        , (Tree, 'x')
        ]

boxed :: [(MapField, Char)]
boxed = [ (Grass, ' ')
        , (Road, '▓')
        , (Desert, '░')
        , (Water, '~')
        , (Obstacle, '█')
        , (Tree, '©')
        ]

mkCharStyle :: [(MapField, Char)] -> (MapField -> Char)
mkCharStyle = undefined

simpleStyle = mkCharStyle simpleChars
asciiStyle = mkCharStyle simpleChars
boxedStyle = mkCharStyle simpleChars
