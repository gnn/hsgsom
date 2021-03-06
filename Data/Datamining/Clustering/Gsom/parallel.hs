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

module Data.Datamining.Clustering.Gsom.Parallel(
phase, run) where

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

-- | The immutable configuration shared by every worker thread.
data Config = Config {gT :: Double
, lR :: Int -> Double
, kF :: Double -> Int -> Double
, cfGrow :: Bool
, radius :: Int -> Double
, step :: TVar Int
, queue :: TVar Inputs
, cfL :: TVar Lattice
, table :: TVar Table
}

-- | @'phase' n parameters inputs@ will update the given @lattice@ by
-- executing one phase of the GSOM algorithm with the given @inputs@
-- and @parameters@ using @n@ threads.
phase :: Int -> Phase -> Lattice -> Inputs -> IO Lattice
phase n ps lattice is = do
  count <- atomically $ newTVar 0
  l <- atomically $ newTVar lattice
  sequence_ (replicate
    (passes ps)
    (pass threads config{step = count, cfL = l} is))
  atomically $ readTVar l where
    config = Config {gT = growthThreshold ps $ dimension is
    , lR = flip (adaption (learningRate ps)) steps
    , kF = kernelFunction (kernel ps)
    , cfGrow = grow ps
    , radius = \s -> (1 - fI s / fI steps ) * fI (neighbourhoodSize ps)
    -- Intentionally undefined empty because they should be defined locally
    -- in each pass
    ,cfL = undefined, step = undefined, queue = undefined, table = undefined
    }
    threads = if (grow ps) then 1 else n
    fI = fromIntegral
    steps = passes ps * length is

-- | @'pass' n config inputs lattice@ will make one pass over the given
-- @inputs@ spawning @n@ threads, adding the missing fields to @config@
-- and modifying the wrapped lattice.
pass :: Int -> Config -> Inputs -> IO ()
pass n conf is = do
  queue <- atomically $ newTVar is
  table <- atomically $ newTVar $ IM.empty
  alive <- spawn n $ work conf{table=table, queue=queue}
  atomically $ do {ts <- readTVar alive; if ts > 0 then retry else return ()}


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
work :: Config -> IO ()
work config = do
  i <- atomically $ do
    is <- readTVar $ queue config
    if null is
      then return Nothing
      else writeTVar (queue config) (tail is) >> return (Just $ head is)
  maybe (return ()) (consume config) i

-- | @'consume' q l t i@ consumes the input @i@, and then goes back to
-- work.
consume :: Config -> Input -> IO ()
consume config i = do
  key <- atomically $ do
    winner <- readTVar (cfL config) >>= bmu i
    im <- readTVar (table config)
    let ks = IM.keys im
    let k = if null ks then 0 else head ks - 1
    writeTVar (table config) $ IM.insert k (winner, i) im
    return k
  atomically $ do
    s <- modifyTVar (step config) (+1)
    winner <- liftM (fst.(IM.! key)) $ readTVar (table config)
    modifyTVar (table config) $ IM.delete key
    let r' = radius config s
    affected <- neighbourhood winner $ round r'
    mapM_ (update i (lR config s) (kF config r')) affected
    l <- readTVar $ cfL config
    (ln, grown) <- if cfGrow config
      then updateError winner i >> vent l winner (gT config)
      else return $! (l, [])
    forM_ (map snd affected) (checkMin (table config))
    writeTVar (cfL config) ln
  work config

checkMin :: TVar Table -> Node -> STM ()
checkMin t' n = do
  w <- readTVar $ weights n
  t <- readTVar t'
  ks<- liftM (map fst) $ filterM (smaller w) (IM.assocs t)
  let tn = foldr (IM.adjust $ newNode n) t ks
  unless (null ks) (writeTVar t' tn) where
  smaller w (k, (n', i)) = do
    w' <- readTVar $ weights n'
    let {d' = distance w' i; d = distance w i}
    boundary <- boundaryNode n'
    return (d' < d || (d' == d && boundary))
  newNode n' (n,i) = (n',i)

modifyTVar :: TVar a -> (a -> a) -> STM a
modifyTVar v f = do
  x <- readTVar v
  writeTVar v $ f x
  return $! f x

-- | Since a complete run of the GSOM algorithm means running a number
-- of @'Phases'@ this is usually the main function used.  @run n phases
-- lattice inputs@ runs the GSOM algorithm by running the @phases@ in
-- the order specified, each time making passes over @inputs@ and using
-- the produced @'Lattice'@ as an argument to the next phase.  The
-- phases are run using @n@ worker threads.  The initial @'Lattice'@,
-- @lattice@ may be constructed with the @'newRandom'@ and the
-- @'newCentered'@ functions.
run :: Int -> Phases -> Lattice -> Inputs -> IO Lattice
run n ps lattice is = foldM f lattice ps where
  f l p = phase n p l is

