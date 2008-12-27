-- | This module contains everything concerning the lattice which is build
-- buy gsom.

module Gsom.Grid where 

-- Standard modules
import System.Random(Random, RandomGen, randomRs, split)

-- Private modules
import Gsom.Input(Input, Inputs, dimension, distance)

-- | The nodes of a gsom are either Leafs, signalling neighbours of boundary 
-- nodes or they are actual nodes with an id, a weight vector, 
-- an error value and a list of neighbouring nodes.
data Node = Leaf | Node { 
  id              :: Int
, neighbours      :: Nodes
, node_error      :: Double
, weight          :: Input
} 

instance Show Node where 
  show Leaf = "(Leaf)"
  show node = " " ++
    "( w = " ++ (show $ weight node) ++ 
    ", E = " ++ (show $ node_error node) ++ " ) "

type Nodes = [Node]


-- | For non a lattice is just a list of nodes.
type Lattice = Nodes 

-- | @'node' weight neighbours@ creates a node with the specified parameters.
node :: Int -> Input -> Nodes -> Node
node id ws ns = Node id ns 0 ws

new :: RandomGen g => g -> Inputs -> Lattice
new g is = [a, b, c, d] where 
  a = node 0 (weights 0) [Leaf, Leaf, b, d]
  b = node 1 (weights 1) [a, Leaf, Leaf, c]
  c = node 2 (weights 2) [a, b, Leaf, Leaf]
  d = node 3 (weights 3) [Leaf, a, c, Leaf]
  weights n = take (dimension is) $ randomRs (gs!!n) (0, 1)
  gs g = let (g1, g2) = split g in g1 : gs g2 
