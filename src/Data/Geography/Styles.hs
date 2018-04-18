module Data.Geography.Styles where

import Data.Geography

import Data.Map as M

-- | Default char if unknown 'MapField' is encountered
defaultChar = '?'

-- | Correspoinding characters
simpleChars :: [(MapField, Char)]
simpleChars = [ (Grass, 'G')
              , (Road, 'R')
              , (Desert, 'D')
              , (Water, 'W')
              , (Obstacle, 'O')
              , (Tree, 'T')
              ]

-- | Basic ASCII symbols
ascii :: [(MapField, Char)]
ascii = [ (Grass, ' ')
        , (Road, '+')
        , (Desert, '*')
        , (Water, '~')
        , (Obstacle, '#')
        , (Tree, 'x')
        ]

-- | Extended ASCII symbols
boxed :: [(MapField, Char)]
boxed = [ (Grass, ' ')
        , (Road, '▓')
        , (Desert, '░')
        , (Water, '~')
        , (Obstacle, '█')
        , (Tree, '©')
        ]

-- | Create transformation from 'MapField' to 'Char'
--
-- TODO: implement
mkCharStyle :: [(MapField, Char)] -> (MapField -> Char)
mkCharStyle = undefined

simpleStyle = mkCharStyle simpleChars
asciiStyle = mkCharStyle simpleChars
boxedStyle = mkCharStyle simpleChars

-- | Create transformation from 'Char' to 'MapField'
--
-- TODO: implement
mkLoadStyle :: [(MapField, Char)] -> (Char -> MapField)
mkLoadStyle = undefined

loadSimpleStyle = mkLoadStyle simpleChars
loadAsciiStyle = mkLoadStyle simpleChars
loadBoxedStyle = mkLoadStyle simpleChars
