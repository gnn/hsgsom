-- |This module contains everything concerning the input to gsom.

module Gsom.Input(
  Input, Inputs
, bounds, dimension, normalize, unnormalize
, distance, (*.), (.*), (<+>), (<->)
) where 

------------------------------------------------------------------------------
-- Standard modules
------------------------------------------------------------------------------

import Data.List(foldl1', transpose)

------------------------------------------------------------------------------
-- Utility functions on lists of inputvectors
------------------------------------------------------------------------------

-- | For now our input vectors are just lists of doubles.
type Input = [Double]
type Inputs = [Input]

-- | Normalizes input vectors.
-- @'normalize' inputs@ takes the given list of input vectors @inputs@ and 
-- returns a list of input vectors where each component is in @[0,1]@.
-- If you want to unnormalize the input vectors use @'bounds'@ and 
-- @'unnormalize'@.
normalize :: Inputs -> Inputs
normalize is = map (map scale . zip3 mins maxs) is where
  scale (min', max', n) = if min' == max' then 0 else (n - min')/(max' - min')
  mins = map minimum is'
  maxs = map maximum is'
  is' = transpose is

-- | Calculates the bounds of the input vector components.
-- @'bounds' inputs@ returns a list of pairs where having the pair @(min, max)@
-- at index i means that the @min@ is the minimum over the components at index 
-- i of the input vectors in @inputs@ while @max@ is the respective maximum.
-- It's horrible but should go through the input list in one pass.
bounds :: Inputs -> [(Double, Double)]
bounds is = foldl1' (mapUncurry2 min max) is' where
  dupZip xs = zip xs xs
  is' = map dupZip is
  mapUncurry2 f g xs = (map $ ap f g) . padZip xs 
  ap f g ((a,b),(c,d)) = (f a c, g b d)

-- | Unnormalizes the given input vectors @inputs@ assuming that it's bounds
-- prviously where @bounds@.
unnormalize :: Inputs -> [(Double, Double)] -> Inputs
unnormalize is bnds = map (map f . zip bnds) is where   
  f ((min',max'), n) = if min' == max' then min' else n*(max'-min')+min'

-- | Calculating the dimension of a given set of inputs just means finding 
-- the length of the longest input vector.
dimension :: Inputs -> Int
dimension = maximum . map length

------------------------------------------------------------------------------
-- Utility functions working on single inputvectors
------------------------------------------------------------------------------

-- | @'distance' i1 i2@ calculates the euclidean distance between 
-- @i1@ and @i2@. If @i1@ and @i2@ have different lengths, excess 
-- values are ignored.
distance :: Input -> Input -> Double
distance i1 = sqrt . sum . zipWith (\a b -> (a-b)**2) i1

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

------------------------------------------------------------------------------
-- Processing functions. Not exported.
------------------------------------------------------------------------------

-- | Zips two lists, but instead of truncating the longer one to the length
-- of the shortert one the shorter one is padded with elements from the 
-- suffix of the longer one which is exceeding the length of the shorter one.
padZip :: [a] -> [a] -> [(a, a)]
padZip xs ys = 
  let (lx, ly) = (length xs, length ys) in uncurry zip $ case compare lx ly of
    EQ -> (xs,ys)
    GT -> (xs, ys ++ drop ly xs)
    LT -> (xs ++ drop lx ys, ys)

