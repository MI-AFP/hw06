module Data.GeographySpec (spec) where

import Test.Hspec

import Data.QuadTree
import Data.Geography
import qualified Data.Geography.Styles as Styles


emptyMap :: Map
emptyMap = QuadTree
         { _qtTree   = Empty
         , _qtWidth  = 0
         , _qtHeight = 0
         }

simpleMap :: Map
simpleMap = QuadTree
          { _qtTree   = Leaf Grass
          , _qtWidth  = 4
          , _qtHeight = 2
          }

complexMap1 :: Map
complexMap1 = QuadTree
            { _qtTree   = Node (Leaf Grass) (Leaf Desert) (Leaf Water) (Leaf Tree)
            , _qtWidth  = 8
            , _qtHeight = 6
            }

complexMap2 :: Map
complexMap2 = QuadTree
           { _qtTree   = Node (Leaf Obstacle) (Leaf Road) (Leaf Water) (Leaf Tree)
           , _qtWidth  = 4
           , _qtHeight = 4
           }

spec :: Spec
spec = do
   describe "displayMap + mkCharStyle" $ do
     it "displays empty map" $
       displayMap emptyMap Styles.simpleStyle `shouldBe` [""]
     it "displays simple map" $ do
       displayMap simpleMap Styles.simpleStyle `shouldBe` ["GGGG",
                                                           "GGGG"]
       displayMap simpleMap Styles.asciiStyle  `shouldBe` ["    ",
                                                           "    "]
       displayMap simpleMap Styles.boxedStyle  `shouldBe` ["    ",
                                                           "    "]
     it "displays complex maps" $ do
       displayMap complexMap1 Styles.simpleStyle `shouldBe` ["GGGGDDDD",
                                                             "GGGGDDDD",
                                                             "GGGGDDDD",
                                                             "WWWWTTTT",
                                                             "WWWWTTTT",
                                                             "WWWWTTTT"]
       displayMap complexMap1 Styles.boxedStyle  `shouldBe` ["    ░░░░",
                                                             "    ░░░░",
                                                             "    ░░░░",
                                                             "~~~~©©©©",
                                                             "~~~~©©©©",
                                                             "~~~~©©©©"]
       displayMap complexMap2 Styles.simpleStyle `shouldBe` ["OOOORRRR",
                                                             "OOOORRRR",
                                                             "OOOORRRR",
                                                             "WWWWTTTT",
                                                             "WWWWTTTT",
                                                             "WWWWTTTT"]
       displayMap complexMap2 Styles.asciiStyle  `shouldBe` ["####++++",
                                                             "####++++",
                                                             "####++++",
                                                             "~~~~xxxx",
                                                             "~~~~xxxx",
                                                             "~~~~xxxx"]
   describe "newCoords" $ do
     it "can make step to North (UP)" $ do
       newCoords (Coords 2 2) North `shouldBe` Coords 2 1
       newCoords (Coords 2 (-2)) North `shouldBe` Coords 2 (-3)
       newCoords (Coords 0 5) North `shouldBe` Coords 0 4
     it "can make step to South (DOWN)" $ do
       newCoords (Coords 2 2) South `shouldBe` Coords 2 3
       newCoords (Coords 2 (-2)) South `shouldBe` Coords 2 (-1)
       newCoords (Coords 0 5) South `shouldBe` Coords 0 6
     it "can make step to East (LEFT)" $ do
       newCoords (Coords 2 2) East `shouldBe` Coords 1 2
       newCoords (Coords 2 (-2)) East `shouldBe` Coords 1 (-2)
       newCoords (Coords 0 5) East `shouldBe` Coords (-1) 5
     it "can make step to West (RIGHT)" $ do
       newCoords (Coords 2 2) West `shouldBe` Coords 3 2
       newCoords (Coords 2 (-2)) West `shouldBe` Coords 3 (-2)
       newCoords (Coords 0 5) West `shouldBe` Coords 1 5
   describe "getDirection" $ do
     it "computes direction from two different points" $ do
       getDirection (Coords 2 2) (Coords 2 1) `shouldBe` Just North
       getDirection (Coords 2 2) (Coords 2 3) `shouldBe` Just South
       getDirection (Coords 2 2) (Coords 1 2) `shouldBe` Just East
       getDirection (Coords 2 2) (Coords 3 2) `shouldBe` Just West
     it "returns Nothing if points are the same" $
       getDirection (Coords 2 2) (Coords 2 2) `shouldBe` Nothing
