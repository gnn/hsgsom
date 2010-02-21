#! /usr/bin/env runhaskell
-- | Just tests.

module Main where

import Data.List(transpose)
import Prelude hiding (Either(..))

import Test.QuickCheck

-- Modules which are only imported so that functions of them can be tested.
import Data.Datamining.Clustering.Gsom.Input(bounds
  , normalize
  , unnormalize
  , (<->))

data Alignment = Left | Right

-- | @'format' alignment separator table@ treats each element of table as
-- one line and each string in each line as column. Columns are then
-- formatted to be aligned according to alignment and separated by separator.
-- Each column is made large enough to fit its largest element.
format :: [[String]] -> Alignment -> String -> [[String]]
format table alignment separator = map (zipWith pad lengths) table where
  lengths = map (foldr (max.length) 0) (transpose table)
  pad n s = (case alignment of
    Left  -> (s ++  )
    Right -> (  ++ s)
    $  replicate (n - length s) ' ' ) ++ separator

-- | @'formatLabels' labeled_list@ formats the labels in the given list
-- so that the label column is big enough for every label, the labels are
-- left aligned and there is a colon before each test result.
formatLabels :: [(String, a)] -> [(String, a)]
formatLabels ps = zip formatted (map snd ps) where
  formatted = map head $ format table Left ": "
  table = map ((:[]).fst) ps

main :: IO ()
main = mapM_ (\(label, test) -> putStr label >> test) tests

-- | All the tests which should be performed along with their labels.
tests :: [(String, IO ())]
tests = formatLabels
  [ ("normalize/idempotent", test prop_normalize_idempotent)
  , ("normalize/bounds", test prop_normalize_bounds)
  , ("unnormalize.normalize/id", test prop_unnormalize_normalize_id)
  ]

-- For testing it is more convenient to have a version of normalize which
-- doesn't need to have the bounds as a paramter but calculates them
-- implicitly using bounds.
normalize' is = normalize (bounds is) is

-- Normalize should be idempotent.
prop_normalize_idempotent s = (normalize'. normalize') s == normalize' s

-- The result of normalize should contain only values between 0 and 1.
prop_normalize_bounds s = all and . map (map test) $ normalize' s where
 test x = x>=0 && x<=1

-- First normalizing and then unnormalizing a list should give the list back,
-- but we have to accomodate for floating point precision errors.
-- This test is horrible so I should try to reformulate it somehow.
prop_unnormalize_normalize_id s =
  foldl max 0 (map (foldl (max.abs) 0) $
    zipWith (<->) ((unnormalize bs . normalize bs) s) s) < 1*10**(-13)
      where bs = bounds s
