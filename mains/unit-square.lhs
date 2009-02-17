#! /usr/bin/env runhaskell

This is just a small script to test whether running the gsom implenentation
gives reasonable results. It generates points distributed uniformly among
a the two dimensional grid [0,0]x[1,1] and tries two quantize those using 
the gsom algorithm. So at first we have to get some boilerplate out of the
way.


> module Main where

> import Control.Concurrent.STM
> import Data.List
> import Data.Ratio
> import Data.Time.Clock.POSIX
> import System.IO
> import System.Random

> import Data.Datamining.Clustering.Gsom.Lattice
> import Data.Datamining.Clustering.Gsom.Node
> import Data.Datamining.Clustering.Gsom.Phase

We will need a helper function to generate a supply of independent random 
number generators from a single one.

> gs :: RandomGen g => g -> [g]
> gs g = let (g', gn) = split g in g' : gs gn

First a few random points distributed uniformly in the unit square.

> randomSquare :: RandomGen g => g -> Int -> Inputs
> randomSquare g n = let f = randomRs (0,1) in
>   take (n^2) $ zipWith (\x y -> [x,y]) (f $ gs g !! 0) (f $ gs g !! 1)

And then a few random points distributed uniformly in the unit cube.

> randomCube :: RandomGen g => g -> Int -> Inputs
> randomCube g n = let 
>   f = randomRs (0,1) 
>   rs = gs g in
>   take n $ zipWith3 (\x y z -> [x,y,z]) 
>     (f $ rs !! 0) 
>     (f $ rs !! 1) 
>     (f $ rs !! 2)

Since we have random points, we also want nonrandom points, so here's
a function to get points in the unit square which form a regular grid.

> grid :: Int -> Inputs
> grid n' = let
>   n = fromIntegral n'
>   supply = map fromRational [ x | x <- [0,1%(n-1)..1]] in
>   take (n'^2) [ [x,y] | x <- supply, y <- supply]

Our main function just runs the gsom algorithm with the defaults.

> main = do
>   stamp <- liftM show getPOSIXTime 
>   g <- getStdGen
>   let is = randomCube g 100000
>   writeFile ("data/" ++ stamp ++ ".is") $ dumpInputs is
>   lattice <- newCentered (dimension is)
>   result <- run defaults lattice is
>   putWeights result >>= writeFile ("data/" ++ stamp ++ ".os")

And one function to format a list of inputs so that we can just dump it into 
a file and load plot it with gnuplot.

> dumpInputs :: Inputs -> String
> dumpInputs = unlines . map (unwords . map show)

