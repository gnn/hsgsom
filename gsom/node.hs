-- | This module contains everything concerning the nodes stored in a 
-- lattice which comprises a gsom. All impure functions in here should
-- be inside the STM monad because nodes are the smallest thing we have 
-- inside a gsom and transacton granularity should be controlled on a 
-- higher level.

module Gsom.Node(
    module Control.Concurrent.STM
  , module Control.Monad
  , Neighbours, Node(..), Nodes
  , isLeaf, isNode, neighbourhood, node, propagate, putNode, spawn
  , unwrappedNeighbours, update, updateError) where 


------------------------------------------------------------------------------
-- Standard modules
------------------------------------------------------------------------------

import Control.Concurrent.STM
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe

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

instance Eq Node where
  Leaf == Leaf = True 
  Node{iD = id1} == Node{iD = id2} = id1 == id2
  _ == _ = False

-- | A node's neighbours are stored in fields of type @Neighbours@.
type Neighbours = [TVar Node]

-- | The type of neighbourhoods. Wherever a neighbourhood of a node
-- is neede, this type should be used.
-- A @Neighbourhood@ consits of a list of pairs of nodes and their 
-- discrete grid distance from the source of the neighbourhood.
-- The source node is the only one with distance @0@ while immediat 
-- neighbours get distance one and so on.
type Neighbourhood = [(Int, Node)]

------------------------------------------------------------------------------
-- Creation
------------------------------------------------------------------------------

-- | @'node' id weights neighbours@ creates a node with the specified 
-- parameters and initial quantization error of @0@.
node :: Int -> Input -> Neighbours -> STM Node
node iD weights neighbours = do
  wrappedWeights <- newTVar weights
  initialError <- newTVar 0
  return $! Node iD initialError wrappedWeights neighbours

------------------------------------------------------------------------------
-- Modifying nodes
------------------------------------------------------------------------------

-- | @'update' input learning_rate kernel neighbour@ updates 
-- the weights of the @neighbour@ according to the formula:
--
-- * @\weight -> weight + learning_rate * (kernel d) (input - weight)@
update :: Input -> Double -> (Int -> Double) -> (Int, Node) -> STM ()
update input lr k (d,n) = modify n weights adjust where 
    adjust w = w <+> (lr * k d) .* (input <-> w)

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
propagate :: Node -> Nodes -> STM()
propagate node affected = do
  let factor = fromIntegral $ length affected
  error <- readTVar $ quantizationError node
  modify node quantizationError (/2)
  mapM_ (\n -> modify n quantizationError (+ 0.5 * error / factor)) affected

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
-- A neighbourhood size of @0@ means that only the given node will be 
-- an element of the returned set while a size of one will return the 
-- given node and it's immediate neighbours and so on.
-- It's not very efficient so you shouldn't try big neihbourhood sizes.
-- The returned neighbourhood always includes @node@.
neighbourhood :: Node -> Int -> STM Neighbourhood
neighbourhood Leaf _ = 
  error "in neighbhourhood: neighbourhood shouldn't be called on leaves."
neighbourhood node radius = iterate ( 
  \pairs -> do
    unwrapped <- pairs
    let border = last . groupBy ((==) `on` fst) $ unwrapped
    newBorder <- mapM iNs border
    return $! (
      nubBy ((==) `on` snd) . 
      (unwrapped ++)        . 
      filter (isNode . snd) . 
      concat ) newBorder)
  (return $! [(0,node)]) !! radius where
    iNs (d,n) = mapM (liftM ((,) $ d+1) . readTVar) (neighbours n)

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

-- | When a new node is spawned we need to calculate it's new weight vector.
-- If the new node is spawned from parent p in direction d and p has a 
-- neighbour n in the direction d' opposite to d then the new weight 
-- vector nw is calculated according to the formula:
--
-- * @nw = 2 * ('weights' p) - ('weights' n)@.
--
-- In all other cases there exists exactly one neighbour of the new node.
-- Let this neighbour be called n and let d' be the direction in which we 
-- have to go to reach this neighbour from the new node. Let s then be 
-- the child of the new node's parent p in direction d'.
-- The new weights are then calculated according to the formula:
--
-- * @nw = p + n - s
newWeight :: Node -> Int -> STM ()
newWeight node d = let 
  w = weights node 
  ns = neighbours node in do
  parent <- readTVar $ ns !! invert d
  sibling <- readTVar $ neighbours parent !! invert d 
  wp <- readTVar $ weights parent
  if isNode sibling 
    then do
      ws <- readTVar $ weights sibling 
      writeTVar w $ 2 .* wp <+> ws
    else do
      candidates <- mapM readTVar $ (map (ns !!)) [left d, right d]
      let d' = fromJust $ findIndex isNode candidates
      wn <- readTVar (ns !! d') >>= readTVar . weights
      ws <- readTVar (neighbours parent !! d') >>= readTVar . weights
      writeTVar w $ wp <+> wn <-> ws
     
-- | Used to modify the fields of a node with.
modify :: Node -> (Node -> TVar a) -> (a -> a) -> STM ()
modify node selector modification = let var = selector node in
  readTVar var >>= writeTVar var . modification

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

