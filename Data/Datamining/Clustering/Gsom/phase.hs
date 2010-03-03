------------------------------------------------------------------------------
-- |
-- Module       : Data.Datamining.Clustering.Gsom.Node.Lattice
-- Copyright    : (c) 2009 Stephan Günther
-- License      : BSD3
--
-- Maintainer   : gnn.github@gmail.com
-- Stability    : experimental
-- Portability  : non-portable (requires STM)
--
-- The GSOM Algorithm can be split up in multiple sequentially executed
-- @'Phase'@s. Each of these phases makes a certain number of passes over
-- the inputs. While doing so each @'phase'@ modifies a given @'Lattice'@
-- according to a certain set of specified parameters.
-- This module contains the definition of the @'Phase'@ type, a few default
-- instances and the functions needed to run a single @'phase'@ or to
-- @'run'@ a sequence of @'Phase'@s.
------------------------------------------------------------------------------

module Data.Datamining.Clustering.Gsom.Phase(
  Phase(..), Phases
-- | The three default phases of the GSOM algorithm. They all use the
-- bubble kernel and a linear learning rate decrease.
, defaultFirst, defaultSecond, defaultThird, defaults
, growthThreshold
, phase
, run
, Kernel(..), kernelFunction
, LearningRate(..), adaption
) where

------------------------------------------------------------------------------
-- Standard modules
------------------------------------------------------------------------------

import Control.Concurrent.STM
import Control.Monad
import Data.List

------------------------------------------------------------------------------
-- Private Modules
------------------------------------------------------------------------------

import Data.Datamining.Clustering.Gsom.Input
import Data.Datamining.Clustering.Gsom.Lattice hiding (grow)
import Data.Datamining.Clustering.Gsom.Node

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- | This datatype encapsulates all the parameters needed to be known to
-- run one phase of the GSOM algorithm.
data Phase = Phase {
  -- | The number of passes which are to be made over the input vectors.
  -- Since each step of the phase consumes one input vector, the overall
  -- number of steps the phase will have will be:
  --
  -- * @steps = passes * length inputs@
  passes :: Int,
  -- | The initial size of the neighbourhood affected by weight adaption.
  -- This will decrease linearly while the phase is executed.
  neighbourhoodSize :: Int,
  -- | The function used to calculate the learning rate in each of the
  -- phase's steps.
  -- During each step @learninRate currentStep maxSteps@ is fed the
  -- number (starting from zero) of the current step as the first
  -- argument and the total number of steps the phase will have as the
  -- second argument to calculate the learning rate which will be in
  -- effect for this phase.
  learningRate :: LearningRate,
  -- | The kernel function. It is used in conjunction with the learning
  -- rate to adjust weight adaption.
  -- @kernel currentNeighbourhoodsize gridDistance@ should take the
  -- neighbourhood size which is in effect during the current step and
  -- a nodes grid distance from the winning node.
  -- The neighbourhood size will be a real number due to the linear decrease.
  kernel :: Kernel,
  -- | The @grow@ flag determines whether this is a growing phase or not.
  -- If this is @False@ then no new nodes will be grown.
  grow :: Bool,
  -- | The spread factor is used to calculate the growth threshold according
  -- to the formula:
  --
  -- * @GT = - sqrt('d')*ln('spreadFactor')@
  --
  -- where @d@ is the input dimension.
  spreadFactor  :: Double
} deriving (Read, Show)

type Phases = [Phase]

-- The predefined Kernel Functions.
data Kernel =
  -- | The bubble kernel is essentially the identity,
  -- i.e. it has no effect.
  Bubble |
  -- | Let @s@ be the neighbourhood size currently in effect
  -- and @d@ be the grid-distance of the current node to the winning node
  -- then this kernel calculates a factor to control weight adaption with
  -- the following formula:
  --
  -- * @gaussian s d = exp(d^2/(2*s^2))@
  Gaussian deriving (Eq, Enum, Ord, Read, Show)

-- | Returns the kernel function associated with the given kernel.
kernelFunction :: Kernel -> Double -> Int -> Double
kernelFunction Bubble = bubble
kernelFunction Gaussian = gaussian

-- The predefined learning rate adaption functions. Their parameters are
-- used as the initial learning rate.
data LearningRate =
  -- | The linear learning rate reduction function. If you supply it with
  -- the initial learning rate @lr@ it uses the following formula where
  -- @step@ is the current step the phase is in and @steps@ is the overall
  -- number of steps the phase will take:
  --
  -- *@linear lr step steps = lr * (1-step/steps)@
  Linear Double |
  -- | The inverse time learning rate reduction function. Given an initial
  -- learning rate of @lr@, a maximum number of steps of @steps@ and the
  -- current step number beeing @step@, the formula is:
  --
  -- *@inverseAge lr step steps = lr * steps / (steps + 100 * step)@
  InverseAge Double deriving (Read, Show)

-- | Returns the learning rate adaption function associated with the given
-- type of learning rate.
adaption :: LearningRate -> Int -> Int -> Double
adaption (Linear d) = linear d
adaption (InverseAge d) = inverseAge d

------------------------------------------------------------------------------
-- Running phases
------------------------------------------------------------------------------

-- | Parameters which have to be passed around between the functions
-- used to execute a phase.
data Config = Config {gT :: Double  -- ^ Growth Threshold
, lR :: Int -> Double               -- ^ Learning Rate (Function of the step)
, kF :: Double -> Int -> Double     -- ^ Kernel Function
, gR :: Bool                        -- ^ Growing Flag
, rS :: Int -> Double               -- ^ Radius (Function of the step)
, sT :: TVar Int                    -- ^ Current step number
, wL :: TVar Lattice                -- ^ wrapped Lattice
}

-- | @'phase' parameters inputs@ will update the given @lattice@ by
-- executing one phase of the GSOM algorithm with the given @inputs@
-- and @parameters@.
phase :: Phase -> Lattice -> Inputs -> IO Lattice
phase ps lattice is = do
  s <- atomically $ newTVar 0
  l <- atomically $ newTVar lattice
  sequence_ (replicate
    (passes ps)
    (pass
      Config{gT = growthThreshold ps $ dimension is
      , lR = flip (adaption (learningRate ps)) steps
      , kF = kernelFunction (kernel ps)
      , gR = grow ps
      , rS = \x -> (1 - fI x / fI steps ) * fI (neighbourhoodSize ps)
      , sT = s
      , wL = l}
      is))
  atomically $ readTVar l where
  pass config is = mapM_ (consume config) is
  steps = passes ps * length is
  fI = fromIntegral

-- | @'consume' confi i@ consumes the input @i@.
consume :: Config -> Input -> IO ()
consume config i = do
  winner <- atomically $ readTVar (wL config) >>= bmu i
  atomically $ do
    s <- modifyTVar (sT config) (+1)
    l <- readTVar $ wL config
    let r = rS config s
    affected <- neighbourhood winner $ round r
    mapM_ (update i (lR config s) (kF config r)) affected
    nL <- if gR config
      then liftM fst $ updateError winner i >> vent l winner (gT config)
      else return $! l
    writeTVar (wL config) nL

modifyTVar :: TVar a -> (a -> a) -> STM a
modifyTVar v f = do
  x <- readTVar v
  writeTVar v $ f x
  return $! f x

-- | Since a complete run of the GSOM algorithm means running a number of
-- @'Phases'@ this is usually the main function used.
-- @run phases lattice inputs@ runs the GSOM algorithm by running the
-- @phases@ in the order specified, each time making passes over @inputs@
-- and using the produced @'Lattice'@ to as an argument to the next phase.
-- The initial @'Lattice'@, @lattice@ may be constructed with the
-- @'newRandom'@ and the @'newCentered'@ functions.
run :: Phases -> Lattice -> Inputs -> IO Lattice
run ps lattice is = foldM f lattice ps where
  f l p = phase p l is

------------------------------------------------------------------------------
-- Predefined Kernels
------------------------------------------------------------------------------

bubble :: Double -> Int -> Double
-- | The simplest kernel, which essentially does nothing.
-- It always evaluates to @1@ thus having no effect on updating weights.
bubble _ _ = 1

-- | A gaussian kernel. The neighbourhood size @s@ currently in effect
-- controls the bell width while the distance @d@ of the current node
-- is used as the actual kernel argument. Thus the formula is:
--
-- * @gaussian s d = exp(d^2/(2*s^2))@
gaussian :: Double -> Int -> Double
gaussian s d = exp $ fromIntegral d ** 2 / (2 * s**2)

------------------------------------------------------------------------------
-- Predefined learning rate updating functions
------------------------------------------------------------------------------

-- Functions to update the learning rate. Used by the constructors of
-- @'LearningRate'@

-- | The linear learning rate reduction function. If you supply it with
-- the initial learning rate @lr@ it uses the following formula where
-- @step@ is the current step the phase is in and @steps@ is the overall
-- number of steps the phase will take:
--
-- *@linear lr step steps = lr * (1-step/steps)@
linear :: Double -> Int -> Int -> Double
linear lr step steps = lr * (1 - fromIntegral step / fromIntegral steps)

-- | The inverse time learning rate reduction function. Given an initial
-- learning rate of @lr@, a maximum number of steps of @steps@ and the
-- current step number beeing @step@, the formula is:
--
-- *@inverseAge lr step steps = lr * steps / (steps + 100 * step)@
inverseAge :: Double -> Int -> Int -> Double
inverseAge lr step steps = let fI = fromIntegral in
  lr * fI steps / (fI steps + 100 * fI step)

------------------------------------------------------------------------------
-- Predefined phases
------------------------------------------------------------------------------


-- | The default first phase is the only growing phase. It makes 5
-- passes over the input, uses an initial learning rate of 0.1 and
-- a starting neighbourhood size of 3. The @'spreadFactor'@ is set
-- to 0.1.
defaultFirst :: Phase
defaultFirst = Phase {
  passes = 5,
  neighbourhoodSize = 3,
  learningRate = Linear 0.1,
  kernel = Bubble,
  grow = True,
  spreadFactor = 0.1
}

-- | The default for the second phase is a smoothing phase making 50
-- passes over the input vectors with a learning rate of 0.05 and an
-- initial neighbourhood size of 2. Since there is no node growth the
-- @'spreadFactor'@ is ignored and thus set to 0.
defaultSecond :: Phase
defaultSecond = Phase {
  passes = 50,
  neighbourhoodSize = 2,
  learningRate = Linear 0.05,
  kernel = Bubble,
  grow = False,
  spreadFactor = 0
}

-- | The default for the third and last phase is a smoothing phase making
-- 50 passes over the input vectors with a learning rate of 0.01 and
-- an initial neighbourhood size of 1. Since there is no node growth the
-- @'spreadFactor'@ is ignored and thus set to 0.
defaultThird :: Phase
defaultThird = Phase {
  passes = 50,
  neighbourhoodSize = 1,
  learningRate = Linear 0.01,
  kernel = Bubble,
  grow = False,
  spreadFactor = 0
}

-- | This is the list of the three default phases of the GSOM algorithm.
defaults :: Phases
defaults = [defaultFirst, defaultSecond, defaultThird]

------------------------------------------------------------------------------
-- Auxiliary Functions
------------------------------------------------------------------------------

-- | Calculates the growth threshold as explained in the documentation
-- for @'Phase'@.
growthThreshold :: Phase -> Int -> Double
growthThreshold ps d =
  negate $ sqrt (fromIntegral d) * log (spreadFactor ps)

