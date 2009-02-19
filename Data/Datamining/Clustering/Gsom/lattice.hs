-- | This module contains everything concerning the lattice which is build
-- by gsom.

module Data.Datamining.Clustering.Gsom.Lattice(
  Lattice,
  newCentered, newRandom, 
  bmu, grow, vent,
  nodes, 
  putLattice, putWeights) where 

------------------------------------------------------------------------------
-- Standard modules
------------------------------------------------------------------------------

import Control.Concurrent.STM
import Control.Monad
import Data.List
import Data.Map(Map(..))
import qualified Data.Map as Map
import Data.Maybe
import System.Random

------------------------------------------------------------------------------
-- Private modules
------------------------------------------------------------------------------

import Data.Datamining.Clustering.Gsom.Coordinates
import Data.Datamining.Clustering.Gsom.Input
import Data.Datamining.Clustering.Gsom.Node

------------------------------------------------------------------------------
-- The Lattice type
------------------------------------------------------------------------------

-- | The lattice type. Since global access to nodes is needed they're 
-- stored in a 'Data.Map' indexed by their coordinates.
type Lattice = Map Coordinates (TVar Node)

------------------------------------------------------------------------------
-- Creation
------------------------------------------------------------------------------

-- | @'newRandom' g dimension@ creates a new minimal lattice where weights are 
-- randomly initialized with values between 0 and 1 using the random number 
-- generator @g@ and with the weight vectors having the specified @dimension@.
newRandom :: RandomGen g => g -> Int -> IO Lattice
newRandom g dimension = let 
  gs g = let (g1, g2) = split g in g1 : gs g2
  weights = [randomRs (0, 1) g' | g' <- gs g]
  in new weights dimension

-- | @'newNormalized' dimension@ creates a new minimal lattice where weights 
-- are initialized with all components having the value @0.5@ the and with 
-- the weight vectors havin length @dimension@.
newCentered :: Int -> IO Lattice
newCentered = new (cycle [cycle [0.5]])

------------------------------------------------------------------------------
-- Reading
------------------------------------------------------------------------------

-- | Returns the nodes stored in lattice.
nodes :: Lattice -> STM Nodes
nodes = mapM readTVar . Map.elems

-- | @'bmu' input lattice@ returns the best matching unit i.e. the node with
-- minimal distance to the given input vector.
bmu :: Input -> Lattice -> IO Node
bmu i l = liftM (filter isNode) (atomically (nodes l)) >>= (\l' -> 
  let ws = readTVar.weights in case l' of
    [] -> error "error in bmu: empty lattices shouldn't occur."
    (x:xs) -> 
      foldM (\n1 n2 -> atomically $ do
        w1 <- ws n1
        w2 <- ws n2
        return $! if distance i w1 <= distance i w2 
          then n1 else n2) 
      x xs
  )

------------------------------------------------------------------------------
-- Manipulating
------------------------------------------------------------------------------

-- | @'grow' lattice node@ will create new neighbours for every Leaf 
-- neighbour of the given @node@ and add the created nodes to @lattice@.
-- It will return the list of spawned nodes and the new lattice containing
-- every node created in the process of spawning. 
grow :: Lattice -> Node -> STM (Lattice, Nodes)
grow lattice node = do
  holes <- liftM (findIndices isLeaf) (unwrappedNeighbours node)
  newLattice <- foldM (flip spawn node) lattice holes 
  spawned <- unwrappedNeighbours node >>= (\ns -> return $! map (ns !!) holes)
  return $! (newLattice, spawned)

-- | Used to spawn only a particular node. Returns the new lattice 
-- containing the spawned node.
-- @'spawn' lattice parent direction@ will create a new node as a 
-- neighbour of @parent@ at index @direction@, making @parent@ the neighbour 
-- of the new node at index @invert direction@ with the new node having an.
spawn :: Lattice -> Node -> Int -> STM Lattice
spawn _  Leaf _ = error "in spawn: spawn called with a Leaf parent."
spawn lattice parent direction = let 
  spawnCoordinates = neighbour (location parent) direction 
  nCs = neighbourCoordinates spawnCoordinates in do
  -- firs we have to check whether there are already some TVars existing
  -- at the locations of the neighbours of the new node and create those
  -- don't exist yet.
  newLattice <- foldM (\m k -> if not $ Map.member k m
      then newTVar Leaf >>= (\v -> return $! Map.insert k v m)
      else return $! m) lattice nCs
  -- after creating all the necessary neighbours we can get create the new 
  -- node with it's neighbours and calculate it's new weight vector
  let ns = specificElements newLattice nCs
  result <- node (neighbour (location parent) direction) [] ns
  writeTVar (fromJust $ Map.lookup spawnCoordinates lattice) result
  newWeight result direction
  return $! newLattice

-- | @'vent' lattice node growthThreshold@ will check the accumulated error 
-- of the @node@ against the given @growthThreshol@ and will do nothing if 
-- the errror value is below the growth threshhold. Otherwise it will either 
-- spawn new nodes or it will propagate the accumulated error value to it's 
-- neighbours, depending on whether the node is a boundary node or not.
-- If new nodes are spawned they will added to @lattice@.

vent :: Lattice -> Node -> Double -> STM Lattice
vent _ Leaf _  = error "in vent: vent called with a Leaf as argument."
vent lattice node gt = do 
  qE <- readTVar $ quantizationError node
  if qE > gt then do 
    ns <- unwrappedNeighbours node
    let leaves = findIndices isLeaf ns
    (newLattice, affected) <- if null leaves
      then liftM ((,) lattice) (neighbourhood node 1 >>= mapM (return . snd))
      else grow lattice node
    propagate node affected
    return $! newLattice
    else return $! lattice
------------------------------------------------------------------------------
-- Internal
------------------------------------------------------------------------------

-- | Generates a new @'Lattice'@ given the supply of @weights@ with each node
-- having a weight vector of the given @dimension@.
new :: Inputs -> Int -> IO Lattice
new ws dimension = let 
  origin = (0,0)
  nodeCoordinates = origin : neighbourCoordinates origin
  leafCoordinates = 
    nub (concatMap neighbourCoordinates nodeCoordinates) \\ nodeCoordinates 
  in atomically $ do
  -- create a map with the TVars for the initial nodes
  lattice <- foldM (\m k -> do
    v <- newTVar Leaf
    return $! Map.insert k v m) Map.empty (nodeCoordinates ++ leafCoordinates)
  -- now that we have all the nodes we need to create the actual non leaf
  -- nodes present in the starting map and write them into the corresoonding
  -- TVars.
  let nodeTVars = specificElements lattice nodeCoordinates
  nodes <- sequence $ zipWith3 node 
    nodeCoordinates 
    (map (take dimension) ws)
    (map (specificElements lattice . neighbourCoordinates) nodeCoordinates)
  zipWithM_ writeTVar nodeTVars nodes
  return $! lattice

specificElements :: Ord k => Map k a -> [k] -> [a]
specificElements m = map (fromJust . flip Map.lookup m)

------------------------------------------------------------------------------
-- Output
------------------------------------------------------------------------------

putLattice :: Lattice -> IO String
putLattice lattice = do
  ns <- atomically (nodes lattice) >>= liftM concat . mapM putNode
  return $ unlines ("Lattice: " : ("  Count: " ++ show (Map.size lattice)) : 
    map ("    " ++ ) ns)

putWeights :: Lattice -> IO String
putWeights lattice = do
  ws <- atomically $ nodes lattice >>= 
    filterM (return.isNode) >>= 
    mapM (readTVar . weights) 
  return $! 
    unlines $
    map (unwords . map show) 
    ws
  
