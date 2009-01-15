module Gsom(Gsom(..), Parameters(..), Input, Inputs) where

import System.Random

------------------------------------------------------------------------------
-- Standard Modules
------------------------------------------------------------------------------

import Control.Monad(foldM, liftM, when)
import Control.Concurrent.STM

------------------------------------------------------------------------------
-- Private Modules
------------------------------------------------------------------------------

import Gsom.Lattice(Lattice(..), bmu, newCentered, newRandom, putLattice, vent)
import Gsom.Node(neighbourhood, update)
import Gsom.Input(Input, Inputs)

-- | These are the parameters needed to create a gsom which are needed in every 
-- iteration and do not change while the algorithm runs.
data Parameters = Parameters {
  -- | The alpha value is used to gradually reduce the learning rate in each 
  -- iteration of the GSOM algorithm. Reduction is done according to the 
  -- formula:
  --
  -- * @newLearningRate = alpha * f(n) * oldLearningRate@
  -- * @f(n) = (1-R/n)
  --
  -- where @n@ is the number of nodes currently in the map and @R@ is 
  -- currently taken to be @3.8@, as specified in the paper by Alahakoon and 
  -- Halgamuge. 
  alpha         :: Double
, -- | The input as well as the weight vector dimension. This is needed so 
  -- that the growth threshhold may be calculated.
  d             :: Int
, -- | The spread factor is used to calculate the growth threshold according 
  -- to the formula GT = - sqrt('d')*ln('spreadFactor').
  spreadFactor  :: Double
}

-- | At each iteration step the gsom consists of the lattice of current nodes, 
-- the current learning rate and the parameters used to create the map.
data Gsom = Gsom {
  lattice       :: Lattice
, learningRate  :: Double
, parameters    :: Parameters
}

-- | @'pass' parameters learningRate lattice inputs grow @ will refine the 
-- given @lattice@ by making one pass over the given @inputs@ with the 
-- GSOM algorithm using the specified @parameters@.
-- If @grow@ is set to true it will be a growin pass otherwise it will be 
-- smoothing pass.
pass :: Parameters -> Double -> Lattice -> Inputs -> Bool -> IO Lattice
pass parameters learningRate lattice is grow = 
  liftM snd $ foldM consume (learningRate, lattice) is where 
    gt = growthThreshold parameters 
    neighbourhoodSize = if grow then 3 else 1 
    consume (lr, l) i = do
      winner <- bmu i lattice
      atomically $ do
        affected <- neighbourhood winner neighbourhoodSize
        update i lr affected
        when grow (mapM_ (\n -> vent l n gt) affected)
        nodeCount <- readTVar (count l)
        return (updateLearningRate (alpha parameters) nodeCount lr, l)

-- | Used to update the learning rate. See parameters for details.
updateLearningRate :: Double -> Int -> Double -> Double
updateLearningRate alpha nodeCount lr = alpha * f * lr where
  f = (1 - 3.8) / fromIntegral nodeCount

-- | Calculates the growth
growthThreshold :: Parameters -> Double
growthThreshold ps = 
  negate $ sqrt (fromIntegral $ d ps) * log (spreadFactor ps)
