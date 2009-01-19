-- | This module contains everything concerning the nodes stored in a 
-- lattice which comprises a gsom. All impure functions in here should
-- be inside the STM monad because nodes are the smallest thing we have 
-- inside a gsom and transacton granularity should be controlled on a 
-- higher level.

module Gsom.Node(
    module Control.Concurrent.STM
  , module Control.Monad
  , Node(..), Nodes
  , isLeaf, isNode, neighbourhood, node, propagate, putNode, spawn
  , unwrappedNeighbours, update, updateError) where 


------------------------------------------------------------------------------
-- Standard modules
------------------------------------------------------------------------------

import Control.Concurrent.STM
import Control.Monad(liftM, liftM2, mapM, unless)
import Data.List(findIndex, findIndices, nub, sortBy)
import Data.Maybe(fromJust)

------------------------------------------------------------------------------
-- Private modules
------------------------------------------------------------------------------

import Gsom.Input(Input, distance, (<+>), (<->), (.*)) 

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

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
  , -- | The node's neighbours.
  neighbours :: Neighbours}
type Nodes = [Node]

-- | A node's neighbours are stored in fields of type @Neighbours@.
type Neighbours = [TVar Node]

instance Eq Node where
  Leaf == Leaf = True 
  Node{iD = id1} == Node{iD = id2} = id1 == id2
  _ == _ = False

------------------------------------------------------------------------------
-- Creation
------------------------------------------------------------------------------

-- | @'node' id weights neighbours@ creates a node with the specified 
-- parameters.
node :: Int -> Input -> Neighbours -> STM Node
node iD weights neighbours = do
  wrappedWeights <- newTVar weights
  initialError <- newTVar 0
  return $! Node iD initialError wrappedWeights neighbours

------------------------------------------------------------------------------
-- Modifying nodes
------------------------------------------------------------------------------

-- | @'setNeighbours' node nodes@ sets the neighbours of @node@ to @nodes@. 
setNeighbours :: Node -> Nodes -> STM Node
setNeighbours n ns = do
  mapM_ (uncurry writeTVar) (zip (neighbours n) ns)
  return n

-- | @'update' input learning_rate nodes@ updates 
-- the weights of the nodes in @nodes@ according to the formula
--
-- * @\weight -> weight + learning_rate * (input - weight)@
update :: Input -> Double -> Nodes -> STM ()
update input lr nodes = mapM_ (\n -> let w = weights n in 
  readTVar w >>= writeTVar w . adjust) 
  nodes where 
    adjust w = w <+> lr .* (input <-> w)

-- | @updateError node input@ updates the @'quantizationError'@ of @node@.
-- The new error is just the old error plus the distance of the @node@'s 
-- weight vector from @input@.
updateError :: Node -> Input -> STM()
updateError n i = let qE = quantizationError n in do
  old <- readTVar qE
  w <- readTVar $ weights n
  writeTVar qE (old + distance w i)

-- | Used to spawn only a particular node. Returns the spawned node.
-- @'spawn' parent id direction@ will create a new node as a 
-- neighbour of @parent@ at index @direction@, making @parent@ the neighbour 
-- of the new node at index @invert direction@ with the new node having an 
-- @'iD'@ of @id@.
spawn :: Int -> Node -> Int -> STM Node
spawn id parent direction = do
  -- find an existing sibling. He is just used to get the TVar the parent is 
  -- stored in, so we can put this TVar in the neighbour list. 
  uWs <- unwrappedNeighbours parent
  let siblingIndex = fromJust $ findIndex isNode uWs
  sibling <- readTVar (neighbours parent !! siblingIndex)
  let back = (invert direction, neighbours sibling !! invert siblingIndex)
  -- generate the new forward neighbour. It should NEVER exist prior to 
  -- spawning a node.
  forward <- liftM2 (,) (return direction) (newTVar Leaf)
  -- fetch the left and right neighbours or create new leaves if they don't 
  -- exist.
  leftright <- mapM get [left, right]
  let 
    ns = map snd $ sortBy
      (\p1 p2 -> compare (fst p1) (fst p2)) (forward : back : leftright)
  result <- node id [] ns
  writeTVar (neighbours parent !! direction) result
  newWeight result direction
  return result
    where 
      -- the get function takes a direction and returns the neighbour in that 
      -- direction if it exists or a new TVar Leaf otherwise
      get f = let newDirection = f direction in do 
        sibling <- readTVar (neighbours parent !! newDirection)
        liftM2 (,) (return newDirection) $ if isLeaf sibling
          then newTVar Leaf
          else return (neighbours sibling !! direction)

-- | @'propagate' node@ propagates the accumulated error of the given @node@ 
-- to it's neighbours.
propagate :: Node -> STM()
propagate node = do
  let factor = 1 + length (neighbours node)
  nodes <- liftM (node :) (unwrappedNeighbours node)
  errorSum <- liftM sum (mapM (readTVar.quantizationError) nodes)
  let newError = errorSum / fromIntegral (length nodes)
  mapM_ (flip writeTVar newError . quantizationError) nodes

------------------------------------------------------------------------------
-- Querying node properties and such
------------------------------------------------------------------------------

-- | @'isLeaf' node@ returns @'True'@ if the given node is a @'Leaf'@ and 
-- @'False'@ otherwise.
isLeaf, isNode :: Node -> Bool
isLeaf Leaf = True
isLeaf _    = False
-- | @'isNode' node@ returns @'False'@ if the given node is a @'Leaf'@ and 
-- @'True'@ otherwise.
isNode      = not.isLeaf

-- | Calculates the neighbourhood of the given size of the given node.
-- It's not very efficient so you shouldn't try big neihbourhood sizes.
-- The returned neighbourhood includes @node@.
neighbourhood :: Node -> Int -> STM Nodes
neighbourhood Leaf _ = 
  error "in neighbhourhood: neighbourhood shouldn't be called on leaves."
neighbourhood node size = liftM nub $ iterate ( 
  \wrappedNodes -> do
    ns <- wrappedNodes
    newNeighbours <- mapM readTVar . concatMap neighbours $ ns 
    return $ ns ++ filter (not.isLeaf) newNeighbours) 
  (return [node]) !! size

-- | @'unwrappedNeighbours' node@ returns the list of neighbours of the 
-- given @node@.
-- Note that neighbours is unwrapped, i.e. the returned list hast type 
-- @'Nodes'@ not @TVar 'Nodes'@.
unwrappedNeighbours :: Node -> STM Nodes
unwrappedNeighbours = mapM readTVar . neighbours

------------------------------------------------------------------------------
-- Utility functions. Not exportet.
------------------------------------------------------------------------------

-- | Inverts a direction, i.e. given an index @i@ representing a direction,
-- it calculates the index representing the backwards direction.
-- The rectangular lattice structure is hardcoded as the number 4. For 
-- generalt structures with n neighbours we should have the formula:
--
-- * invert i = (i+n/2) `mod` n
invert, left, right :: Int -> Int
invert i = (i+2) `mod` 4

-- | @'left' direction@ returns the index which points to the left of 
-- @direction@.
left i = (i+1) `mod` 4

-- | @'left' direction@ returns the index which points to the left of 
-- @direction@.
right i = (i-1) `mod` 4

-- | This should probably belong into input.hs. 
checkBounds :: Input -> Input
checkBounds i = let (min',max') = (minimum i, maximum i) in
  if min' < 0 || max' > 1 
  then replicate (length i) 0.5 
  else i

-- | When a new node is spawned we need to calculate it's new weight vector
-- This is either done with with (w1+w2)/2 if the node is between two nodes
-- or 2*w1-w2 where w1 is the weight the parent which spawned the node 
-- and w2 is the weight of one of the parents neighbours.
-- For this we need the spawned node as an argument as well as the 
-- direction in which it was spawned so we can 
-- calculate and write it's new weight vector:
-- @'newWeight' node parent direction@
newWeight :: Node -> Int -> STM ()
newWeight node direction = let 
  weight = weights node 
  writeWeight = writeTVar weight . checkBounds  
  in do
  -- if the node already has a weight vector don't do anything.
  -- Update should be called in this case.
  uW <- readTVar weight
  unless (null uW) (error 
    "in newWeight: node already has a weight vector. Update should be called.")
  -- get the new nodes neighbours
  unNs <- unwrappedNeighbours node
  -- check whether the new node is in between two nodes
  let l = unNs !! left direction
  let r = unNs !! right direction
  let p = unNs !! invert direction
  -- if it is, use first formula
  if isNode l && isNode r then do
    w1 <- readTVar $ weights l
    w2 <- readTVar $ weights r
    writeTVar weight $ 0.5 .* (w1 <+> w2)
    -- if its not, check for parents neigbours
    else do
      upNs <- unwrappedNeighbours p
      let pl = upNs !! left direction
      let pr = upNs !! right direction
      let pp = upNs !! invert direction
      wp <- readTVar $ weights p
      if isNode pp then do
        wpp <- readTVar $ weights pp
        let w = 2 .* wp <-> wpp
        writeWeight w
        else do
          let candidates = filter isNode [pl, pr]
          case candidates of 
            [] -> error "in newWeight: new node has only one neighbour."
            (x:xs) -> do
            wx <- readTVar $ weights x
            let w = 2.* wp <-> wx
            writeWeight w

------------------------------------------------------------------------------
-- Output
------------------------------------------------------------------------------

putNode :: Node -> IO [String]
putNode Leaf = return ["Leaf"]
putNode node = atomically $ do
  let id = "iD : " ++ show (iD node)
  e <- liftM (("error : " ++) . show) (readTVar $ quantizationError node) 
  w <- liftM (("weights: " ++ ) . show) (readTVar $ weights node)
  ns <- liftM (show . map iD . filter isNode) 
              (mapM readTVar (neighbours node))
  return $ "Node:" : map ("  " ++) [id, e, w, ns]

