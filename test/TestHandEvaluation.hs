module TestHandEvaluation where

import Data.Maybe
import Data.List
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
propCombinationTripTwoPairHigh :: [Card] -> Bool
propCombinationTripTwoPairHigh cards
    | elem 3 lengthList = comb == ThreeOfAKind
    | pairs >= 2        = comb == TwoPairs
    | pairs == 1        = comb == Pair
    | otherwise         = comb == HighCard
  where
    lengthList = map length (groupBy ((==) `on` rank) cards)    
    Just (comb, cards') = tripsTwoPairHigh cards
    pairs = length $ filter (==2) lengthList
  




-- What do we want to test?
-- gives right combination     CHECK
-- gives right rank on combination
-- gives best possible kickers



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
combinationsTests = localOption (QuickCheckTests 5000) $
                    testGroup "Combination Tests"
  [ testProperty "prop_name" propAverage                             -- test test
  , testCase     "Ace value" $ assertBool "AceLow < Ace" (rankValueAceLow Ace < rankValue Ace) -- aceValue high/low
  , testProperty "removeCards" propRemCards
  , testProperty "tripsTwoPairHigh combination" propCombinationTripTwoPairHigh
  ]

