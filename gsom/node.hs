{-# LANGUAGE FlexibleInstances #-}
-- | This module contains everything concerning the nodes stored in a 
-- lattice which comprises a gsom. This module depends on the language 
-- extension FlexibleInstances to specialize the @'Show'@ and @'Eq'@ 
-- instances for nodes with value type NodeData.
module Gsom.Node where 

import Gsom.Input(Input)

-- | This is the type of data stored in a gsom node which is not related to 
-- neighbourhood information. It is used so that @'Node'@ can be parameterized 
-- over a type and made an instance of @'Functor'@.
data NodeData = NodeData { 
  -- | Used to uniquely identify nodes. For new nodes this should be set to
  -- the current size of the gsom+1 to ensure that @'nodeId'@ is unique.
  nodeId    :: Int
, -- | The error value the node has accumulated so far.
  nodeError :: Double
, -- | The nodes weight vector. This is the center of the voronoi cell the 
  -- node represents.
  nodeWeight:: Input
} 

-- | The nodes of a gsom are either Leafs, signalling neighbours of boundary 
-- nodes or they are actual nodes with an associated value and a list of 
-- neighbouring nodes.
data Node a = Leaf | Node { value :: a, neighbours :: Nodes a}
type Nodes a = [Node a]
type GsomNode = Node NodeData
type GsomNodes = [GsomNode]

instance Show (Node NodeData) where 
  show Leaf = "(Leaf)"
  show node = " " ++
    "( w = " ++ (show $ nodeWeight $ value node) ++ 
    ", E = " ++ (show $ nodeError $ value node) ++ " ) "

instance Eq (Node NodeData) where
  (==) n1 n2 = (nodeId . value) n1 == (nodeId . value) n2

instance Functor Node where 
  fmap _ Leaf = Leaf 
  fmap f n    = Node (f $ value n) (map (fmap f) (neighbours n))

-- | @'node' weight neighbours@ creates a node with the specified parameters.
node :: Int -> Input -> GsomNodes -> GsomNode
node id ws ns = Node (NodeData id 0 ws) ns

