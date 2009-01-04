-- | This module contains everything concerning the nodes stored in a 
-- lattice which comprises a gsom. 
module Gsom.Node(
  module Control.Concurrent.STM
  , Node(..), Nodes
  , isLeaf, isNode, node, setNeighbours) where 

-- Modules from the standard library.
import Control.Concurrent.STM

-- Modules private to this library.
import Gsom.Input(Input) 

-- | The nodes of a gsom are either Leafs, signalling neighbours of boundary 
-- nodes or they are actual nodes with a few associated values and a list of 
-- neighbouring nodes.
data Node = Leaf | 
  Node { 
  -- | Used to uniquely identify nodes. For new nodes this should be set to
  -- the current size of the gsom+1 to ensure that @'iD'@ is unique. 
  -- Since @'iD'@ also shouldn't change after the node is created it is not 
  -- stored in a @'TVar'@.
  iD :: Int
  , -- | The quantization error the node has accumulated so far.
  quantizationError :: TVar Double
  , -- | The nodes weight vector. This is the center of the voronoi cell the 
    -- node represents.
  weights :: TVar Input
  , -- | The list of the node's neighbours.
  neighbours :: Nodes}
type Nodes = [TVar Node]

-- | @'node' id weights neighbours@ creates a node with the specified 
-- parameters.
node :: Int -> Input -> Nodes -> STM Node
node iD weights neighbours = do
  wrappedWeights <- newTVar weights
  initialError <- newTVar 0
  return $! Node iD initialError wrappedWeights neighbours

