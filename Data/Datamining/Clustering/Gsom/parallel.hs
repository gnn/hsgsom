------------------------------------------------------------------------------
-- |
-- Module       : Data.Datamining.Clustering.Gsom.Parallel
-- Copyright    : (c) 2009, 2010 Stephan Günther
-- License      : BSD3
--
-- Maintainer   : gnn -dot- code -at- gmail -dot- com
-- Stability    : experimental
-- Portability  : non-portable (requires STM)
--
-- This module contains the necessary modifications of some functions to
-- parallelise a phase of the GSOM Algorithm by using multiple threads.
------------------------------------------------------------------------------

module Data.Datamining.Clustering.Gsom.parallel() where

------------------------------------------------------------------------------
-- Standard modules
------------------------------------------------------------------------------

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List
import Data.Maybe

------------------------------------------------------------------------------
-- Private Modules
------------------------------------------------------------------------------

import Data.Datamining.Clustering.Gsom.Input
import Data.Datamining.Clustering.Gsom.Lattice hiding (grow)
import Data.Datamining.Clustering.Gsom.Node
import Data.Datamining.Clustering.Gsom.Phase hiding (phase, run)

------------------------------------------------------------------------------
-- Running phases
------------------------------------------------------------------------------

-- | A shared table used for bookkeeping purposes. It stores the 'bmu'
-- nodes and the corresponding 'Input' points so that they can be
-- changed safely in between transactions, and retrieved later.
type Table = IntMap (Node, Input)

