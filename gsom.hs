{-# LANGUAGE ExistentialQuantification #-}

module Gsom(Gsom(..), Parameters(..), Input, Inputs) where

import System.Random

import Gsom.Lattice(Nodes)
import Gsom.Input(Input, Inputs)

-- | These are the parameters needed to create a growing self organizing map.
data Parameters = forall g. (RandomGen g) => Parameters {
  -- | The input as well as the weight vector dimension. This is needed so 
  -- that the growth threshhold may be calculated.
  d             :: Int
, -- | The random number generator use whenever we need random values. I think 
  -- those are only needed during grid initialisation so maybe they may be 
  -- dropped
  randomizer    :: g
, -- | The spread factor is used to calculate the growth threshhold according 
  -- to the formula GT = - sqrt('d')*ln('spreadFactor').
  spreadFactor  :: Double
}

-- | At each iteration step the gsom consists of the grid of current nodes, 
-- the current learning rate and the parameters used to create the map.
data Gsom = Gsom {
  grid          :: Nodes
, learningRate  :: Double
, parameters    :: Parameters
}

