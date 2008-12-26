-- |This module contains everything concerning the input to gsom.

module Gsom.Input where 

import Data.List(transpose)

-- | For now our input vectors are just lists of doubles.
type Input = [Double]
type Inputs = [Input]

-- | Normalizing input vectors means scaling each component to be in [0,1].
normalize :: Inputs -> Inputs
normalize is = map (map scale) $ map (zip3 mins maxs) is where
  scale (min', max', n) = if min' == max' then 0 else (n - min')/(max' - min')
  mins = map minimum is'
  maxs = map maximum is'
  is' = transpose is


-- | calculating the dimension of a given set of inputs just means finding 
-- the length of the longest input vector
dimension :: Inputs -> Int
dimension = maximum . map length


