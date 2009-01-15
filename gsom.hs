module Gsom(Gsom(..), Parameters(..), Input, Inputs) where

import System.Random

import Gsom.Lattice(Lattice)
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

