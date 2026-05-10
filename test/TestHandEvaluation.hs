module TestHandEvaluation where


import Types
import Cards
import HandEvaluation

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck



import System.Exit
import Control.Exception

-- HUnit -> tests specifik examples
-- QuickCheck -> tests a lot of cases



main :: IO ()
main = defaultMain combinationsTests


-- just to see how tests is working in tasty
average' :: Float -> Float -> Float
average' a b = (a+b)/2

propAverage :: Float -> Float -> Bool
propAverage a b = average' a b == a/2 + b/2

-- TODO
-- function removeCards
-- function combinationRoot
-- function straightFlush
-- function straight
-- function quidsFullhouse
-- function tripsTwoPairHigh

-- function handData
-- function handCompariosion
-- function winners
-- function winnersWithComb




combinationsTests :: TestTree
combinationsTests = testGroup "Combination Tests"
  [ testProperty "prop_name" propAverage]
