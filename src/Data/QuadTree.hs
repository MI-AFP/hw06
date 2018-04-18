{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.QuadTree where

import Control.Lens

data QuadTree a = QuadTree
                { _qtTree   :: Quadrant a
                , _qtWidth  :: Integer
                , _qtHeight :: Integer
                }
                deriving (Eq, Show, Read)

data Quadrant a = Empty
                | Leaf a
                | Node { _qaTL :: Quadrant a
                       , _qaTR :: Quadrant a
                       , _qaBL :: Quadrant a
                       , _qaBR :: Quadrant a
                       }
                deriving (Eq, Show, Read)

makeLenses ''QuadTree
makeLenses ''Quadrant

instance Functor QuadTree where
  fmap = undefined

instance Foldable QuadTree where
  -- TODO: implement foldr or foldMap
  foldMap = undefined

instance Traversable QuadTree where
  -- TODO: implement traverse or sequenceA
  traverse = undefined

mkQuadTree :: a -> Integer -> Integer -> QuadTree a
mkQuadTree fill width height = undefined

hasElementOn :: QuadTree a -> Integer -> Integer -> Bool
hasElementOn t x y = undefined

getElement :: QuadTree a -> Integer -> Integer -> Maybe a
getElement t x y = undefined

toMatrix :: QuadTree a -> [[a]]
toMatrix = undefined
