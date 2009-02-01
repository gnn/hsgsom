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
> import System.IO
> import System.Random

> import Gsom.Lattice
> import Gsom.Node
> import Gsom.Phase

First a few random points distributed uniformly in the unit square.

> randomSquare :: RandomGen g => g -> Int -> Inputs
> randomSquare g n = let 
>   (gx, gy) = split g 
>   f = randomRs (0,1) in
>   take (n^2) $ zipWith (\x y -> [x,y]) (f gx) (f gy)

Since we have random points, we also want nonrandom points, so here's
a function to get points in the unit square which form a regular grid.

> grid :: Int -> Inputs
> grid n' = let
>   n = fromIntegral n'
>   supply = map fromRational [ x | x <- [0,1%(n-1)..1]] in
>   take (n'^2) [ [x,y] | x <- supply, y <- supply]

Our main function just runs the gsom algorithm with the defaults.

> main = do
>   g <- getStdGen
>   let is = grid 1000
>   writeFile "_T_" $ dumpInputs is
>   lattice <- newCentered (dimension is)
>   result <- run defaults lattice is
>   appendFile "_T_" "\n\n"
>   putWeights result >>= appendFile "_T_"

And one function to format a list of inputs so that we can just dump it into 
a file and load plot it with gnuplot.

> dumpInputs :: Inputs -> String
> dumpInputs = unlines . map (unwords . map show)

