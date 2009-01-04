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
new :: RandomGen g => g -> Inputs -> IO Lattice
new g is = do 
  let gs g = let (g1, g2) = split g in g1 : gs g2
  let weights = \n -> take (dimension is) $ randomRs (0, 1) (gs g !! n)
  references <- atomically $ mapM newTVar (replicate 16 Leaf)
  nodes <- mapM
    (\n -> node n (weights n) (take 4 $ drop (n*4) references))
    [0,1,2,3]
  let 
    neighbours = map (map (\n -> if n < 0 then Leaf else nodes!!n))
      ( [-1, -1, 1, 3] 
      : [0, -1, -1, 2] 
      : [3, 1, -1, -1] 
      : [-1, 0, 2, -1] 
      : [])
  mapM (uncurry setNeighbours) (zip nodes neighbours)
  atomically $ filterM (readTVar >=> return . not . isLeaf) references

-- | @'bmu' input lattice@ returns the best matching unit i.e. the node with
-- minimal distance to the given input vector.
bmu :: Input -> Lattice -> IO Node
bmu i l = atomically $ case l of
    [] -> error "error in bmu: empty lattices shouldn't occur."
    (x:xs) -> 
      foldM (\v1 v2 -> do
        i1 <- (readTVar v1 >>= readTVar.weights)
        i2 <- (readTVar v2 >>= readTVar.weights)
        if (distance i $ i1) <= (distance i $ i2) 
          then return v1 else return v2) 
      x xs >>= readTVar

-- | @'update' input learning_rate ids node@ traverses the whole map updating 
-- the weights of the nodes which have @'nodeId'@s matching those in ids
-- according to the formula: 
-- * @\weight -> weight + learning_rate'.*'(input '<->' weight)@
-- and returns the new gsom.
update :: Input -> Double -> [Int] -> GsomNode -> GsomNode
update i lr ids gsom = fmap (
  \v -> if nodeId v `elem` ids then adjust v else id v) gsom where 
  adjust v = v{nodeWeight = nodeWeight v <+> lr .* (i <-> nodeWeight v)}

