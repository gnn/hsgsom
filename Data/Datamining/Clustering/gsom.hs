------------------------------------------------------------------------------
-- | 
-- Module       : Data.Datamining.Clustering.Gsom
-- Copyright    : (c) 2009 Stephan Günther
-- License      : BSD3
--
-- Maintainer   : gnn.github@gmail.com
-- Stability    : experimental
-- Portability  : non-portable (requires STM)
--
-- This module should contain everything you need to run the GSOM clustering 
-- algorithm. It collects and re-exports all important and needed functions
-- from moduls lower in the hirarchy. 
--
-- Ideally you should never need to look at those modules. If you do need 
-- to do this, it is a design failure and I would appreciate it if you 
-- would drop me a note.
------------------------------------------------------------------------------

module Data.Datamining.Clustering.Gsom (
) where

------------------------------------------------------------------------------
-- Standard modules
------------------------------------------------------------------------------

import Control.Concurrent.STM
import Control.Monad
import Data.Function
import Data.List
import Data.Map(Map(..))
import qualified Data.Map as Map

------------------------------------------------------------------------------
-- Private modules
------------------------------------------------------------------------------

import Data.Datamining.Clustering.Gsom.Coordinates
import Data.Datamining.Clustering.Gsom.Input
import Data.Datamining.Clustering.Gsom.Lattice hiding (bmu)
import Data.Datamining.Clustering.Gsom.Node
import Data.Datamining.Clustering.Gsom.Phase

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- | The clusters generated by GSOM basically consist of three things:
data Cluster = Cluster {
  -- | the vector which best represents all the vectors belonging to this
  -- cluster.
  center :: Input
    -- | the input vectors belonging to this cluster. Note that there's
    -- no guarantee and in fact it is quite improbable that you will
    -- have @('center'@ `'elem'` 'contents')@
, contents :: Inputs
  -- | the coordinates of this cluster
, coordinates :: Coordinates
}

-- | The final clustering which is the result of the GSOM algorithm 
-- is a @'Data.Map'@ mapping @'Coordinates'@ to @'Cluster'@s.
type Clustering = Map Coordinates Cluster

------------------------------------------------------------------------------
-- Creation
------------------------------------------------------------------------------

-- | Computes a clustering induced by the given lattice.
--
-- @'clustering' lattice@ uses the @'weights'@ of the @'nodes'@ 
-- stored in @lattice@ to generate clusters and returns the 
-- @'Clustering'@ storing these clusters. Each non leaf node @n@ in @lattice@
-- cluster @c@ with @('coordinates' c = 'location' n)@ and with @'center' c@ 
-- equal to the weight vector of @n@. Each generated clusters contents are 
-- empty.
clustering :: Lattice -> IO Clustering
clustering l = do
  ns <- atomically $ liftM (filter isNode) (nodes l)
  associations <- atomically $ mapM (\n -> do 
    ws <- readTVar $ weights n
    return $! (location n, ws)) ns
  return $! Map.fromList $ map (\(xy, w) -> 
    (xy, Cluster {center = w, contents = [], coordinates = xy})) associations

-- | @'cluster' inputs clustering@ clusters the given @inputs@ according to 
-- the centers of the clusters in @clustering@. That means each input from
-- @inputs@ is added to the contents of the cluster center it has minimal 
-- distance to.
-- TODO: Implement tiebreaker.
cluster :: Inputs -> Clustering -> Clustering
cluster is cs = foldl' f cs is where
  f cs i = let c = bmu i cs in 
    Map.insert (coordinates c) (c{contents = i : contents c}) cs

------------------------------------------------------------------------------
-- Internal Functions
------------------------------------------------------------------------------

-- | Again computes the best matching unit, only this time it is pure.
bmu :: Input -> Clustering -> Cluster
bmu i = snd . minimumBy (compare `on` (distance i) . center . snd) . Map.assocs
