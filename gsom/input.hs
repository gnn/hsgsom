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

-- | @'distance' i1 i2@ calculates the euclidean distance between i1 and i2
-- If @i1@ and @i2@ have different lengths, excess values are ignored.
distance :: Input -> Input -> Double
distance i1 i2 = sqrt $ sum $ zipWith (\a b -> (a-b)**2) i1 i2


-- | Multiplication of an input vector with a scalar.
infixr 7 .* 
(.*) :: Double -> Input -> Input
(.*) d = map $ \x -> d*x

infixl 7 *.
(*.) :: Input -> Double -> Input
(*.) = flip (.*)

-- | Subtraction and addition of vectors between each other.
infixl 6 <->, <+>

(<+>), (<->) :: Input -> Input -> Input
(<+>) i1 i2 = let 
  front = zipWith (+) i1 i2 
  l1 = length i1
  l2 = length i2 in case signum $ l1 - l2 of
    0   -> front
    -1  -> front ++ drop l1 i2
    1   -> front ++ drop l2 i1
(<->) i1 i2 = i1 <+> (-1) .* i2
