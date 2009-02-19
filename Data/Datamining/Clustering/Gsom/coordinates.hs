-- | Nodes are stored with two dimensional discrete coordinates 
-- uniquely identifying them. Since these coordinates will be used
-- as a key inside a 'Data.Map' and since we need some functions 
-- to translate walking from one node into a certain direction 
-- to walking from one coordinate to another all this functionality
-- will be put into this module.
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

neighbourCoordinates :: Coordinates -> [Coordinates]
neighbourCoordinates cs = map (neighbour cs) directions

