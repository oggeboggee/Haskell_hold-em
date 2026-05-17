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

-- conditional testing
-- own generator
-- lenses ot seperate example specific things that we want to be visible for specifc players


main :: IO ()
main = defaultMain combinationsTests

------------------------------------------------

{-
tests :: TestTree
tests = testGroup "Tests" [ testCase "2 + 2 = 4" $ 2 + 2 @?= 4 ]
-}



-- TODO
-- write an flowchart CHECK
-- organize the function: flow, helpers, helpers to helpers
-- clean and write more clear: winners & handComparision
-- give "javaDoc-comments" to functions and test:

{-
function winners
    - HUnit tests for the most complicated cases (?)
    - Try to get an error, fix it
function handComparision
    - try to compare same hand
    - try to give wrong input, do we need to fix it if it crashes? why/why not?
function value
    - test length with properties
    - test edge-cases
    -
function ifNotFlush
    - test length with properties
    - test edge-cases
    - 
function ifFlush
    - test length with properties
    - test edge-cases
    -
function lastNelem
function maybeFlush
function maybeStraight
function maybeWheel
function checkGroups
function evalGroupedRanks
function groupBySuccCards

   ------  CHECKed under this line ---------

function sortByLength
-}



-- bug found in maybeFlush: if running with aceLowStraight -> Gives cards in wrong order


----------------------groupBySuccCards------------------------------

groupBySuccCardsTest1 :: Bool
groupBySuccCardsTest1 = groupBySuccCards [Card King Hearts, Card Ace Hearts] ==  [[Card King Hearts, Card Ace Hearts]]


--groupBySuccCardsTest2 :: Bool
--groupBySuccCardsTest3 :: Bool
--groupBySuccCardsTest4 :: Bool
--groupBySuccCardsTest5 :: Bool
--groupBySuccCardsTest6 :: Bool
--groupBySuccCardsTest7 :: Bool
--groupBySuccCardsTest8 :: Bool




----------------------sortByLength------------------------------
-- | tests if lists of cards is in falling order of length
sortByLengthPropLength :: [[Card]] -> Bool
sortByLengthPropLength xs = sortByLengthPropHelp (sortByLength xs)


-- | tests if highest list of cards is in decending order
-- | test with inner lists in the same size using gen Lists
sortByLengthPropSize :: Property
sortByLengthPropSize = forAll genLists (sortByLengthPropHelpSize . sortByLength)


-- | tests order of lists, focusing on the length of the inner lists
sortByLengthPropHelp :: [[Card]] -> Bool
sortByLengthPropHelp [] = True
sortByLengthPropHelp [_] = True
sortByLengthPropHelp (c1:c2:rest)
    | length c1 >= length c2 = sortByLengthPropHelp (c2:rest)
    | otherwise = False

sortByLengthPropHelpSize :: [[Card]] -> Bool
sortByLengthPropHelpSize [] = True
sortByLengthPropHelpSize [_] = True
sortByLengthPropHelpSize all@(c1:c2:rest)
    | maximum all == c1 = sortByLengthPropHelpSize (c2:rest)
    | otherwise = False



 -- | generates lists with lists that contains 5 card each
genLists :: Gen [[Card]]
genLists = listOf (vectorOf 5 arbitrary)


-----------------------------------------------------------------
------------------------   TESTTREE  ----------------------------
-----------------------------------------------------------------

combinationsTests :: TestTree
combinationsTests = localOption (QuickCheckTests 5000) $
                    testGroup "Combination Tests"
  [ 
    testProperty "sortByLength length inner Lists" sortByLengthPropLength
  , testProperty "sortByLength size innerLists" sortByLengthPropSize
  , testCase "groupBySuccCardsTest1" $ groupBySuccCardsTest1 @?= True
  ]

-----------------------------------------------------------------
-----------------------------------------------------------------
-----------------------------------------------------------------
hand0 :: Hand
hand0 = [Card Two Hearts, Card Four Spades ]


hand3s :: Hand
hand3s = [ Card Two Spades,  Card Two Clubs, Card Five Clubs, Card Jack Spades ]

-- | sorted
hand4 :: Hand
hand4 = [ Card Two Spades,  Card Two Clubs, Card Two Hearts, Card Five Clubs, Card Jack Spades ]

hand5 :: Hand --Flush
hand5 = [Card Ten Hearts , Card Jack Hearts, Card Queen Hearts, Card King Hearts, Card Ace Hearts]

hand6 :: Hand --Flush
hand6 = [Card Three Hearts, Card Ten Hearts , Card Jack Spades, Card Queen Hearts, Card King Hearts, Card Ace Hearts]

hand7 :: Hand
hand7 = [Card Queen Hearts, Card Queen Spades, Card King Hearts, Card King Spades, Card Ace Hearts ]

hand8 :: Hand --one pair
hand8 = [Card Jack Hearts, Card Jack Spades, Card Queen Hearts, Card King Spades, Card Ace Hearts ]

hand9 :: Hand --two pair
hand9 = [Card Jack Hearts, Card Jack Spades, Card King Hearts, Card King Spades, Card Ace Hearts]

hand10 :: Hand --two pair
hand10 = [Card Jack Hearts, Card Jack Spades, Card Queen Hearts, Card King Spades, Card King Hearts]

hand11 :: Hand --three of a Kind (x1)
hand11 = [Card Jack Clubs, Card Jack Spades, Card Jack Hearts, Card King Spades, Card King Hearts, Card Ace Clubs]

hand12 :: Hand --three of a Kind (x2)
hand12 = [Card Jack Clubs, Card Jack Spades, Card Jack Hearts, Card King Spades, Card King Hearts, Card King Clubs]

hand13 :: Hand --Full House (x2)
hand13 = [Card Jack Clubs, Card Jack Spades, Card Jack Hearts, Card King Spades, Card King Hearts, Card Ace Clubs]

hand14 :: Hand --Full House (x2)
hand14 = [Card Jack Clubs, Card Jack Spades, Card King Hearts, Card King Spades, Card King Hearts, Card Ace Clubs]

hand15 :: Hand -- Quads
hand15 = [Card Two Hearts, Card Two Spades, Card Two Diamonds, Card Two Clubs, Card King Hearts, Card Ace Diamonds, Card Ace Hearts]

hand16 :: Hand -- Quads
hand16 = [Card Two Hearts, Card Two Spades, Card Two Diamonds, Card Two Clubs, Card King Hearts, Card King Spades, Card King Clubs, Card King Diamonds]

hand17 :: Hand -- Straight (5 cards)
hand17 = [Card Three Hearts, Card Four Hearts, Card Five Hearts, Card Six Hearts, Card Seven Spades]

hand18 :: Hand -- Straight (5 cards (all in row))
hand18 = [Card Three Hearts, Card Four Hearts, Card Five Hearts, Card Six Hearts, Card Seven Spades, Card Eight Spades]

hand19 :: Hand --Flush
hand19 = [Card Three Hearts, Card Ten Hearts , Card Jack Hearts, Card Queen Hearts, Card King Hearts, Card Ace Hearts]

hand20 :: Hand -- Straight Flush
hand20 = [Card Three Hearts, Card Four Hearts, Card Five Hearts, Card Six Hearts, Card Seven Hearts]

hand21 :: Hand -- Pair
hand21 = [Card Two Hearts, Card Four Hearts, Card Six Hearts, Card Six Clubs, Card Seven Hearts]

hand22 :: Hand -- Pair
hand22 = [Card Two Hearts, Card Four Hearts, Card Six Hearts, Card Six Clubs, Card Seven Hearts, Card Jack Hearts, Card Queen Hearts]

hand23 :: Hand --Nothing (High Card)
hand23 = [Card Two Hearts, Card Four Hearts, Card Six Hearts, Card Eight Clubs, Card Ten Clubs, Card Jack Diamonds, Card Ace Diamonds]

hand24 :: Hand
hand24 = [Card Five Hearts, Card Eight Hearts, Card Jack Hearts, Card Queen Diamonds, Card Ace Diamonds]

hand25 :: Hand
hand25 = [Card Six Hearts, Card King Hearts]

hand26 :: Hand
hand26 = [Card Seven Hearts, Card Queen Hearts]

hand27 :: Hand
hand27 = [Card Eight Diamonds, Card Queen Clubs]

hand28 :: Hand
hand28 = [Card Five Hearts, Card Six Spades, Card Seven Clubs, Card Eight Diamonds, Card Ace Hearts]

hand29 :: Hand -- Straight Ace Low
hand29 = [Card Ace Diamonds, Card Two Clubs, Card Three Diamonds, Card Four Clubs, Card Five Hearts]

hand30 :: Hand -- Straight Ace Low
hand30 = [Card Ace Diamonds, Card Two Diamonds, Card Three Diamonds, Card Four Diamonds, Card Five Diamonds]

hand31 :: Hand
hand31 = [Card Two Hearts, Card Three Spades, Card Four Hearts, Card Five Clubs]

hand32 :: Hand -- Straight Ace Low
hand32 = [ Card Two Clubs, Card Three Diamonds, Card Four Clubs, Card Five Hearts, Card Ace Diamonds]

---------------------------------
----------- HandLists -----------
handList1 :: [[Card]]
handList1 = [hand25, hand26, hand27]

handList2 :: [[Card]]
handList2 = [[Card Five Hearts, Card King Spades], [Card Eight Hearts, Card King Clubs], [Card Two Spades, Card Three Spades]]

handList3 :: [[Card]]
handList3 = [[Card Ace Hearts, Card King Clubs], [Card Three Hearts, Card Six Clubs]]

----------------------------------

tester :: ([Card] -> (Combination, [Card])) ->  [([Card], (Combination, [Card]))]
tester fun = [ (hand0, fun hand0), 
               (hand1, fun hand1),
               (hand2, fun hand2),
               (hand3, fun hand3),
               (hand4, fun hand4),
               (hand5, fun hand5),
               (hand6, fun hand6),
               (hand7, fun hand7),
               (hand8, fun hand8),
               (hand9, fun hand9),
               (hand10, fun hand10),
               (hand11, fun hand11),
               (hand12, fun hand12),
               (hand13, fun hand13),
               (hand14, fun hand14),
               (hand15, fun hand15),
               (hand16, fun hand16),
               (hand17, fun hand17),
               (hand18, fun hand18),
               (hand19, fun hand19),
               (hand20, fun hand20),
               (hand21, fun hand21),
               (hand22, fun hand22),
               (hand23, fun hand23),
               (hand24, fun hand24),
               (hand25, fun hand25),
               (hand26, fun hand26),
               (hand27, fun hand27),
               (hand28, fun hand28),
               (hand29, fun hand29),
               (hand30, fun hand30)
               ]
