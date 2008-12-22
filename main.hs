module Main where 

import System.Environment(getArgs)

import Gsom

main :: IO ()
main = do 
  arguments <- getArgs
  let input = if null arguments then "input.txt" else head arguments
  return ()

