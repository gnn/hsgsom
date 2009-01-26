-- | The GSOM Algorithm can be split up in multiple sequentially executed
-- phases. Each of these phases makes a certain number of passes over
-- the inputs. While doing so the algorithm modifies a given lattice
-- according to a certain set of specified parameters.
-- This module contains all the functionality needed to run one or more 
-- phases of the GSOM algorithm.
module Gsom.Phase(
  Phase,
  phase
) where 

------------------------------------------------------------------------------
-- Standard modules
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Private Modules
------------------------------------------------------------------------------

import Gsom.Input
import Gsom.Lattice hiding (grow)
import Gsom.Node

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
  learningRate :: Int -> Int -> Double,
  -- | The kernel function. It is used in conjunction with the learning 
  -- rate to adjust weight adaption. 
  -- @kernel currentNeighbourhoodsize gridDistance@ should take the 
  -- neighbourhood size which is in effect during the current step and
  -- a nodes grid distance from the winning node. 
  -- The neighbourhood size will be a real number due to the linear decrease.
  kernel :: Double -> Int -> Double,
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
}

------------------------------------------------------------------------------
-- Running phases
------------------------------------------------------------------------------

-- | @'phase' parameters inputs@ will update the given @lattice@ by 
-- executing one phase of the GSOM algorithm with the given @inputs@ 
-- and @parameters@.
phase :: Phase -> Lattice -> Inputs -> IO Lattice
phase ps lattice is =
  liftM snd $ foldM consume (0, lattice) supply where 
    supply = passes ps `times` is
    gT = growthThreshold ps $ dimension is
    lR x = learningRate ps x steps
    steps = length supply
    fI = fromIntegral
    r x = ((1 - fI x / fI steps ) * fI (neighbourhoodSize ps)) :: Double
    consume (c, l) i = do
      winner <- bmu i lattice
      atomically $ do
        affected <- neighbourhood winner $ round (r c)
        mapM_ (update i (lR c) (kernel ps $ r c)) affected 
        when (grow ps) (updateError winner i >> vent l winner gT)
        nodeCount <- readTVar (count l)
        return $! (c + 1, l)

------------------------------------------------------------------------------
-- Internal functions
------------------------------------------------------------------------------

-- | Calculates the growth threshold as explained in the documentation
-- for @'Phase'@.
growthThreshold :: Phase -> Int -> Double
growthThreshold ps d = 
  negate $ sqrt (fromIntegral d) * log (spreadFactor ps)

-- | @n `times` l@ returns @l@ repeated @n@ times.
-- @l@ has to be finite and because the length of @l@ is calculated
-- in the process.
times :: Int -> [a] -> [a]
n `times` l = take (n * length l) (cycle l)
