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

-- | The type of nodes of a gsom.
data Node =
  -- | They're either Leafs, signalling neighbours of boundary nodes 
  Leaf |
  -- |  or they are actual nodes with a few associated values and a list of 
  -- neighbouring nodes.
  Node { 
  -- | Used to uniquely identify nodes. For new nodes this should be set to
  -- the current size of the gsom+1 to ensure that @'iD'@ is unique. 
  -- Since @'iD'@ also shouldn't change after the node is created it is not 
  -- stored in a @'TVar'@.
  iD :: Int
  , -- | The quantization error the node has accumulated so far.
  quantizationError :: TVar Double
  , -- | The node's weight vector. This is the center of the voronoi cell the 
    -- node represents.
  weights :: TVar Input
  , -- | The list of the node's neighbours.
  neighbours :: [TVar Node]}
type Nodes = [Node]

-- | @'node' id weights neighbours@ creates a node with the specified 
-- parameters.
node :: Int -> Input -> Nodes -> IO Node
node iD weights neighbours = atomically $ do
  wrappedWeights <- newTVar weights
  initialError <- newTVar 0
  wrappedNeighbours <- mapM newTVar neighbours
  return $! Node iD initialError wrappedWeights wrappedNeighbours

-- | @'setNeighbours' node nodes@ sets the neighbours of @node@ to @nodes@. 
setNeighbours :: Node -> Nodes -> IO Node
setNeighbours n ns = atomically $ do
  mapM (uncurry writeTVar) (zip (neighbours n) ns)
  return n

-- | @'isLeaf' node@ returns @'True'@ if the given node is a @'Leaf'@ and 
-- @'False'@ otherwise.
isLeaf, isNode :: Node -> Bool
isLeaf Leaf = True
isLeaf _    = False
-- | @'isNode' node@ returns @'False'@ if the given node is a @'Leaf'@ and 
-- @'True'@ otherwise.
isNode      = not.isLeaf
