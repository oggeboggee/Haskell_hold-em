module TestHandEvaluation where

import Data.Maybe
import Data.Function (on)

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

----------------------------------------
----------------------------------------
----------------------------------------

 -- | properties for function removeCards
propRemCardsAll :: [Card] -> [Card] -> Bool
propRemCardsAll cards _ = removeCards cards cards == []

propRemCardsNone :: [Card] -> [Card] -> Bool
propRemCardsNone cards _ = removeCards [] cards == cards

propRemCards :: [Card] -> [Card] -> Bool
propRemCards cards c2 = (propRemCardsAll cards c2) && (propRemCardsNone cards c2)


-- | properties for function tripsTwoPairHigh
propTrips :: [Card] -> Bool
propTrips cards = undefined
--  where
--    lengthList = map length (groupBy ((==) `on` rank) cards)


-- What do we want to test?
-- the function does only give nothing on a empty list
-- the function gives combination threeOfAKind if we have 3 of same rank
-- the function gives combination twoPair if we have 2 of same rank
-- the function gives combaination HighCard with the highest possible card first in the return-hand


-- maybe False (==1) Nothing (how to use maybe)




-- TODO

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
  [ testProperty "prop_name" propAverage                             -- test test
  , testCase     "Ace value" $ assertBool "AceLow < Ace" (rankValueAceLow Ace < rankValue Ace) -- aceValue high/low
  , testProperty "removeCards" propRemCards
  ]

