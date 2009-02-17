-- |This module contains everything concerning the input to gsom.

module Data.Datamining.Clustering.Gsom.Input(
  Bounds, Input, Inputs
, bounds, dimension, normalize, unnormalize
, distance, (*.), (.*), (<+>), (<->)
) where 

------------------------------------------------------------------------------
-- Standard modules
------------------------------------------------------------------------------

import Data.List

------------------------------------------------------------------------------
-- Utility functions on lists of inputvectors
------------------------------------------------------------------------------

-- | Input vectors are just lists of doubles.
type Input = [Double]
type Inputs = [Input]

-- | The bounds of a list of inputs. Having the tuple @(a,b)@ at index @i@ 
-- in @bounds@ means that the value at index @i@ of each of the input vectors
-- from the inputs which where used to calculate @bounds@ is from the 
-- intervall @[a,b]@.
type Bounds = [(Double, Double)]

-- | Normalizes input vectors.
-- @'normalize' inputs@ takes the given list of input vectors @inputs@ and 
-- returns a list of input vectors where each component is in @[0,1]@.
-- If you want to unnormalize the input vectors use @'bounds'@ and 
-- @'unnormalize'@.
normalize :: Bounds -> Inputs -> Inputs
normalize bs is = map (normalizeVector bs) is where
  normalizeVector bs = map normalizeValue . zip bs
  normalizeValue ((a,b),v) = if a == b then 0 else (v - a)/(b - a)

-- | Calculates the bounds of the input vector components.
bounds :: Inputs -> Bounds
bounds [] = []
bounds (i:is) = foldl' f (dz i) is where
  dz x = zip x x
  f ps [] = ps
  f [] xs = dz xs
  f ((a,b):ps) (x:xs) = let a' = min a x; b' = max b x; t = f ps xs in
    a' `seq` b' `seq` t `seq` (a',b') : t

-- | Unnormalizes the given input vectors @inputs@ assuming that it's bounds
-- prviously where @bounds@.
unnormalize :: Bounds -> Inputs -> Inputs
unnormalize bounds inputs = map (map f . zip bounds) inputs where   
  f ((min',max'), n) = if min' == max' then min' else n*(max' - min')+min'

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
distance i1 = sqrt . sum . zipWith (\a b -> (a - b)**2) i1

-- | Multiplication of an input vector with a scalar.
infixr 7 .* 
(.*) :: Double -> Input -> Input
(.*) d = (force . map ((d*) $!) $!)

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

-- | Forces a whole list. If it wasn't for this function, @'bounds'@ 
-- would blow the stack because only the @'head'@ of the bounds would be fully
-- evaluated while the @'tail'@ would consist of huge thunks of @'min'@ and 
-- @'max'@ applcations.
force :: [a] -> [a]
force [] = []
force (x:xs) = let tail = force xs in x `seq` tail `seq` x:tail

