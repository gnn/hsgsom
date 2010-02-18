------------------------------------------------------------------------------
-- |
-- Module       : Data.Datamining.Clustering.Gsom
-- Copyright    : (c) 2009 Stephan Günther
-- License      : BSD3
--
-- Maintainer   : gnn.github@gmail.com
-- Stability    : experimental
-- Portability  : portable
--
-- The network created by the GSOM algorithm is layed out in two dimensions.
-- Thus one needs to assign two dimensional coordinates to the nodes of the
-- network and for a clustering to the clusters.
--
-- The types defining these coordinates and the functions to handle them belong
-- into this module.
------------------------------------------------------------------------------
module Data.Datamining.Clustering.Gsom.Coordinates where

type Coordinates = (Int, Int)

type Direction = Int

type Directions = [Int]

-- | The list of supported directions. Since we are only dealing with
-- hexagonal lattices, there are only six possible directions.
directions :: Directions
directions = [0..5]

-- | @'neighbour' location direction@ calculates the coordinates of
-- the neighbour of node with location @location@ in direction
-- @direction@.
neighbour :: Coordinates -> Direction -> Coordinates
neighbour coordinates direction
  | direction > 5 = error $
    "in neighbour: direction to big " ++ show direction ++ " (not in [0,5])."
  | otherwise = map
      ((\p1 p2 -> (fst p1 + fst p2, snd p1 + snd p2)) coordinates)
      [(2, 0), (1, 1), (-1, 1), (-2, 0), (-1, -1), (1, -1)]
      !! direction

-- | @'neighbourCoordinates' point@ calculates the list of
-- coordinates which are directly adjacent to @point@.
neighbourCoordinates :: Coordinates -> [Coordinates]
neighbourCoordinates cs = map (neighbour cs) directions

