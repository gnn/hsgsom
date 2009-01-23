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
  -- @kernel gridDistance currentNeighbourhoodsize@ should take a nodes
  -- grid distance from the winning node and the neighbourhood size which
  -- is in effect during the current step. This neighbourhood size will be
  -- a real number due to the linear decrease.
  kernel :: Int -> Double -> Double,
  -- | The @grow@ flag determines whether this is a growing phase or not.
  -- If this is @False@ then no new nodes will be grown.
  grow :: Bool
}


