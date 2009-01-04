-- | This module contains everything concerning the lattice which is build
-- buy gsom.

module Gsom.Lattice where 

-- Standard modules
import Control.Monad(filterM, foldM, (>=>))
import System.Random(Random, RandomGen, randomRs, split)

-- Private modules
import Gsom.Input(Input, Inputs, dimension, distance, (<+>), (<->), (.*))
import Gsom.Node

-- | For now a lattice is just a list of nodes. Every node should be reachable 
-- from every other node so a lattice might as well be represented by one 
-- single node but this approach here is chosen as to able to calculate the
-- current number of nodes simply by doing @'length' lattice@ for a given 
-- lattice.
type Lattice = Nodes

-- | @'new' g inputs@ creates a new minimal lattice where weights are randomly
-- initialized with values between 0 and 1 using the random number generator g
-- and with the weight vectors having dimension equal to the input dimension.
new :: RandomGen g => g -> Inputs -> Lattice
new g is = [a, b, c, d] where 
  a = node 0 (weights 0) [Leaf, Leaf, b, d]
  b = node 1 (weights 1) [a, Leaf, Leaf, c]
  c = node 2 (weights 2) [a, b, Leaf, Leaf]
  d = node 3 (weights 3) [Leaf, a, c, Leaf]
  weights n = take (dimension is) $ randomRs (0, 1) (gs g !! n)
  gs g = let (g1, g2) = split g in g1 : gs g2 

-- | @'bmu' input lattice@ returns the best matching unit i.e. the node with
-- minimal distance to the given input vector.
bmu :: Input -> Lattice -> GsomNode
bmu i l = let weight = nodeWeight . value in case l of
  [] -> error "error in bmu: empty lattices shouldn't occur."
  (x:xs) -> bmu' x xs where
    bmu' current remaining = case remaining of 
      [] -> current
      (x:xs) -> if (distance i $ weight current) <= (distance i $ weight x)
        then bmu' current xs else bmu' x xs 

-- | @'update' input learning_rate ids node@ traverses the whole map updating 
-- the weights of the nodes which have @'nodeId'@s matching those in ids
-- according to the formula: 
-- * @\weight -> weight + learning_rate'.*'(input '<->' weight)@
-- and returns the new gsom.
update :: Input -> Double -> [Int] -> GsomNode -> GsomNode
update i lr ids gsom = fmap (
  \v -> if nodeId v `elem` ids then adjust v else id v) gsom where 
  adjust v = v{nodeWeight = nodeWeight v <+> lr .* (i <-> nodeWeight v)}

