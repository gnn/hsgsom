module Gsom where

import Data.List(transpose)
import System.Random(Random, RandomGen, randomR, split)

-- | a 'GSOM' is a collection of the following values:
-- a list of Nodes,
-- the spread factor, 
-- the dimension of the input data,
-- the learning rate.
-- The latter changes while the algorithm is build
data RandomGen g => GSOM g = GSOM { 
  nodes         :: Nodes,
  spread_factor :: Double, 
  dimension     :: Int, 
  learning_rate :: Double,
  inputs        :: Inputs,
  generator     :: g
} deriving Show

-- | The nodes of a gsom are either Leafs, signalling neighbours of boundary 
-- nodes or they are actual nodes containing a list of mapped input vectors, 
-- a weight vector, an error value and a list of neighbouring nodes.
data Node = Leaf | Node { 
  mapped_vectors  :: Inputs,
  weight          :: Input,
  node_error      :: Double,
  neighbours      :: Nodes
} 

instance Show Node where 
  show Leaf = "(Leaf)"
  show node = " " ++
    "( w = " ++ (show $ weight node) ++ 
    ", E = " ++ (show $ node_error node) ++ " ) "

type Nodes = [Node]

-- | For now our input vectors are just lists of doubles.
type Input = [Double]
type Inputs = [Input]

-- | @'gsom' spread_factor learning_rate inputs generator@ creates a new 'GSOM' 
-- with the specified parameters. The nodes will be initialized to the 
-- default initial size which currently is four nodes since for the 
-- moment I only support a rectangular lattice structure. The dimension
-- will be initialized by checking the maximum length of the input vectors.
gsom :: RandomGen g => Double -> Double -> Inputs -> g -> GSOM g
gsom sf lr ivs g = GSOM {
  spread_factor = sf, learning_rate = lr, inputs = ivs,
  nodes = [a, b, c, d], dimension = d', generator = g1} where
  d' = maximum $ map length ivs
  a = node (take d' rws) [Leaf, Leaf, b, d]
  b = node (take d' $ drop d' rws) [a, Leaf, Leaf, c]
  c = node (take d' $ drop (2*d') rws) [a, b, Leaf, Leaf]
  d = node (take d' $ drop (3*d') rws) [Leaf, a, c, Leaf]
  iT = transpose ivs
  ranges = zip (map minimum iT) (map maximum iT)
  (g1, g2) = split g
  rws = randomRss (take (4*d') $ cycle ranges) g2
  
-- | @'node' weight neighbours@ creates a node with the specified parameters.
-- mapped vectors will be initialized to the empty list and the starting error
-- will be 0.
node :: Input -> Nodes -> Node
node ws ns = Node [] ws 0 ns

-- | @'randomRss' ranges generator@ will generate a list of random numbers 
-- where each list entry is in the range indicated by the pair at the 
-- corresponding position in @ranges@.
randomRss :: (Random a, RandomGen g) => [(a,a)] -> g -> [a]
randomRss ranges g = fst $ foldr f ([], g) ranges where 
  f (a, b) (l, g) = let (x, g') = randomR (a, b) g in
    (x:l, g')


