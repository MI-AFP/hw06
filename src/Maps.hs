module Maps(map01) where

import Data.Geography
import Data.Geography.Styles

-- | Dummy map
map01 = loadMap loadSimpleStyle ["TGGGGGGGGGRGGGGGGGGGGGGWWWWWWGGGDDDDDDDD",
                                 "GGGGGGGGGGRGGGGGTGGGGGWWWWWWWGGGGGDDDDDD",
                                 "GGGTGGGGGGRGGGGGGGGGGGWWWWWWWGGGGGTGDDDD",
                                 "GGOOOOOOOGRGGGGTTGGGGGWWWWWWWGGGGGGGDDDD",
                                 "GGOGGGGGOGRGGGGGGGGGGGGGWWWWWGGGGGTGDDDD",
                                 "GGOOOOOOOGRGGGGGTGGGGGGGGGWWGGGGGGGGGGGG",
                                 "GGGGTTGGGGRRRRRRRRRRRRRGGGWWGGGGGGGGGGGG",
                                 "GGTGGGGGGGGGGGGGGGGGGGRGGGWWGGGGGGGGGGGG",
                                 "GGGGGGGTGGGGGGGGTGGGGGRGGGWWGGGGGGGGGGGG",
                                 "TTGTGGGGGGGGGGGTTGGGGGRGGGWWWWGGGGGGGGGG",
                                 "TTGGGGGGGTGGGDDDGDDDDGRGGGGGWWWGGGGGGGGG",
                                 "TTTTGGGGGGGGGDDDDDDDDGGRGGGGGGGWWGGGGGGG"]
