#! /usr/bin/env runhaskell
-- | Just tests.

module Main where 

import Data.List(transpose)
import Prelude hiding (Either(..))

import Test.QuickCheck

-- Modules which are only imported so that functions of them can be tested.
import Gsom.Input(normalize)

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
-- left aligned and there is a colob before each test result.
formatLabels :: [(String, a)] -> [(String, a)]
formatLabels ps = zip formatted (map snd ps) where 
  formatted = map head $ format table Left ": "
  table = map ((:[]).fst) ps

main :: IO ()
main = mapM_ (\(label, test) -> putStr label >> test) tests

-- | All the tests which should be performed along with their labels.
tests :: [(String, IO ())]
tests = formatLabels 
  $ ("normalize/idempotent", test prop_normalize_idempotent)
  : ("normalize/bounds", test prop_normalize_bounds)
  : []

-- Normalize should be idempotent.
prop_normalize_idempotent s = (normalize.normalize) s == normalize s

-- The result of normalize should contain only values between 0 and 1.
prop_normalize_bounds s = and.(map and).(map $ map test) $ (normalize s) where
 test = (\x -> (x>=0 && x<=1))