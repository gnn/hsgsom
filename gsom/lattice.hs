-- | This module contains everything concerning the lattice which is build
-- by gsom.

module Gsom.Lattice where 

------------------------------------------------------------------------------
-- Standard modules
------------------------------------------------------------------------------

import Control.Monad(filterM, foldM, (>=>))
import System.Random(Random, RandomGen, randomRs, split)

------------------------------------------------------------------------------
-- Private modules
------------------------------------------------------------------------------

import Gsom.Input(Input, Inputs, dimension, distance)
import Gsom.Node

------------------------------------------------------------------------------
-- The Lattice
------------------------------------------------------------------------------

-- | For now a lattice is just a list of nodes. Every node should be reachable 
-- from every other node so a lattice might as well be represented by one 
-- single node but this approach here is chosen as to be able to calculate the
-- current number of nodes simply by doing @'length' lattice@ for a given 
-- lattice.
type Lattice = Nodes

------------------------------------------------------------------------------
-- Creation
------------------------------------------------------------------------------

-- | @'new' g inputs@ creates a new minimal lattice where weights are randomly
-- initialized with values between 0 and 1 using the random number generator g
-- and with the weight vectors having dimension equal to the input dimension.
new :: RandomGen g => g -> Inputs -> IO Lattice
new g is = atomically $ do 
  let gs g = let (g1, g2) = split g in g1 : gs g2
  let weights n = take (dimension is) $ randomRs (0, 1) (gs g !! n)
  nodes <- mapM
    (\n -> node n (weights n) (replicate 4 Leaf))
    [0,1,2,3]
  let 
    neighbours = map (map (\n -> if n < 0 then Leaf else nodes!!n))
      ( [-1, -1, 1, 3] 
      : [0, -1, -1, 2] 
      : [3, 1, -1, -1] 
      : [-1, 0, 2, -1] 
      : [])
  mapM_ (uncurry setNeighbours) (zip nodes neighbours)
  return nodes

------------------------------------------------------------------------------
-- Functions working on the lattice
------------------------------------------------------------------------------

-- | @'bmu' input lattice@ returns the best matching unit i.e. the node with
-- minimal distance to the given input vector.
bmu :: Input -> Lattice -> IO Node
bmu i l = let ws = readTVar.weights in case l of
    [] -> error "error in bmu: empty lattices shouldn't occur."
    (x:xs) -> 
      foldM (\n1 n2 -> atomically $ do
        w1 <- readTVar $ weights n1
        w2 <- readTVar $ weights n2
        if distance i w1 <= distance i w2 
          then return n1 else return n2) 
      x xs


