-- | This module contains everything concerning the lattice which is build
-- by gsom.

module Gsom.Lattice where 

------------------------------------------------------------------------------
-- Standard modules
------------------------------------------------------------------------------

import Control.Monad(filterM, foldM, unless, when, zipWithM_, (>=>))
import Data.List(findIndices, zipWith3)
import System.Random(Random, RandomGen, randomRs, split)

------------------------------------------------------------------------------
-- Private modules
------------------------------------------------------------------------------

import Gsom.Input(Input, Inputs, dimension, distance)
import Gsom.Node

------------------------------------------------------------------------------
-- The Lattice type
------------------------------------------------------------------------------

-- | The lattice type. It has two fields:
data Lattice = Lattice {
  -- | The number of current nodes. Is tracked in a TVar for efficiency 
  -- reasons and so that unique node ids can be created thread safe.
  count :: TVar Int
, -- | The list of nodes. it is also tracked in a TVar so that new nodes can 
  -- be inserted in a thread safe manner. 
  -- In addidion every node should be reachable from every other node. 
  nodes :: TVar Nodes
}

------------------------------------------------------------------------------
-- Creation
------------------------------------------------------------------------------

-- | @'new' g inputs@ creates a new minimal lattice where weights are randomly
-- initialized with values between 0 and 1 using the random number generator g
-- and with the weight vectors having dimension equal to the input dimension.
new :: RandomGen g => g -> Inputs -> IO Lattice
new g is = atomically $ do
  let ids = [0..3]
  let gs g = let (g1, g2) = split g in g1 : gs g2
  let weights = [ take (dimension is) $ randomRs (0, 1) (gs g !! n) | n <- ids]
  -- create the TVars for the initial nodes
  nodes <- sequence $ replicate 4 (newTVar Leaf)
  neighbours <- mapM (mapM (\n -> if n < 0 
    then newTVar Leaf 
    else return $ nodes!!n))
    [ [-1, -1, 1, 3] 
    , [0, -1, -1, 2] 
    , [3, 1, -1, -1] 
    , [-1, 0, 2, -1] ]
  sequence (zipWith3 node ids weights neighbours) >>= 
    zipWithM_ writeTVar nodes
  count' <- newTVar 4
  nodes' <- mapM readTVar nodes >>= newTVar
  return $ Lattice count' nodes'

------------------------------------------------------------------------------
-- Reading
------------------------------------------------------------------------------

-- | @'bmu' input lattice@ returns the best matching unit i.e. the node with
-- minimal distance to the given input vector.
bmu :: Input -> Lattice -> IO Node
bmu i l = atomically (readTVar $ nodes l) >>= (\l' -> 
  let ws = readTVar.weights in case l' of
    [] -> error "error in bmu: empty lattices shouldn't occur."
    (x:xs) -> 
      foldM (\n1 n2 -> atomically $ do
        w1 <- readTVar $ weights n1
        w2 <- readTVar $ weights n2
        if distance i w1 <= distance i w2 
          then return n1 else return n2) 
      x xs
  )

------------------------------------------------------------------------------
-- Manipulating
------------------------------------------------------------------------------

-- | Inserts a node into the lattice and returns the new lattice

insert :: Lattice -> Node -> STM Lattice
insert l@(Lattice c' ns') n = do
  c <- readTVar c'
  ns <- readTVar ns'
  writeTVar c' (c+1)
  writeTVar ns' (n:ns)
  return l

-- | @'grow' node@ will create new neighbours for every Leaf neighbour of 
-- the given @node@. 
grow :: Node -> STM ()
grow node = do
  ns <- unwrappedNeighbours node
  let holes = findIndices isLeaf ns
  mapM_ (spawn (-1) node) holes

-- | @'vent' node growthThreshold@ will check the accumulated error of the 
-- @node@ against the given @growthThreshol@ and will do nothing if 
-- the errror value is below the growth threshhold. Otherwise it will either 
-- spawn new nodes or it will propagate the accumulated error value to it's 
-- neighbours, depending on whether the node is a boundary node or not.
vent :: Node -> Double -> STM ()
vent Leaf _  = error "in vent: vent called with a Leaf as argument."
vent node gt = do 
  qE <- readTVar $ quantizationError node
  when (qE > gt) $ do 
    ns <- unwrappedNeighbours node
    let leaves = findIndices isLeaf ns
    unless (null leaves) (grow node)
    propagate node

