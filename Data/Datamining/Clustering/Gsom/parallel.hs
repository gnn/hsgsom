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

-- | @'spawn' n action@ spawns @n@ worker threads doing action and
-- returns a 'TVar' containing an integer which maintains acount of how
-- many of the spawned threads are still alive.
spawn :: Int -> IO () -> IO (TVar Int)
spawn n action = do
  alive <- atomically $ newTVar n
  replicateM_ n . forkIO $
      action `finally` (
      atomically $ modifyTVar alive $ subtract 1)
  return alive

-- | The worker action. @'work' queue lattice table@ repeatedly takes a
-- point from the @queue@ and acts on it, modfying @lattice@ and using
-- @table@ for storing and retrieving bmus.
work :: TVar Inputs -> TVar Lattice -> TVar Table -> IO ()
work q l t = do
  i <- atomically $ do
    is <- readTVar q
    if null is
      then return Nothing
      else writeTVar q (tail is) >> return (Just $ head is)
  maybe $ return () $ consume q l t $ i

-- | @'consume' q l t i@ consumes the input @i@, and then goes back to
-- work.
consume :: TVar Inputs -> TVar Lattice -> TVar Table -> Input -> IO ()
consume q l t i = do
  key <- atomically $ do
    winner <- bmu i l
    im <- readTVar table
    let ks = keys im
    let k = if null ks then 0 else head ks - 1
    writeTVar table $ IM.insert k winner im
    return k
-- still have to figure out how this is going to work.
    atomically $ do
      affected <- neighbourhood winner $ round (r c)
      mapM_ (update i (lR c) (kernelFunction (kernel ps) $ r c)) affected
      newLattice <- if grow ps
        then updateError winner i >> vent l winner gT
        else return $! l
-- and maybe this last part goes into the work function.
  work q l t

