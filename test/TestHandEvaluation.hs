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

-----------------------------------------------------------------
------------------------   TESTTREE  ----------------------------
-----------------------------------------------------------------

combinationsTests :: TestTree
combinationsTests = localOption (QuickCheckTests 5000) $
                    testGroup "Combination Tests"
  [ 
    testProperty "sortByLength length inner Lists" sortByLengthPropLength
  , testProperty "sortByLength size innerLists" sortByLengthPropSize
  , testCase "groupBySuccCards " $ hUnitGroupBySucc @?= True
  , testProperty "evalGroupedRanksProp" evalGroupedRanksProp
  , testProperty "length property checkGroups" lengthCheckGroupsProp
  , testCase "HUnit checkGroups  "  $ hUnitCheckGroups @?= True
  , testCase "HUnit maybeWheel   "  $ hUnitmaybeWheel  @?= True
  , testCase "HUnit maybeStraight"  $ hUnitMaybeStraight @?= True
  , testProperty "maybeStraight length" lengthStraightprop
  , testCase "HUnit checkGroups  "  $ hUnitMaybeFlush @?= True
  , testProperty "same suit in maybeFlush" sameSuitFlushProp
  ]

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
    - test hands when it could be different kind of combinations at the same time. see if we will get the right one -> HUnit-tests
        
        -- test flush at the same time as fullHouse., find svårigheter in the structure of the code.


    - test length with properties
    - test edge-cases

function ifNotFlush
function ifFlush
function lastNelem



   ------  CHECKed under this line ---------

function maybeFlush
function maybeStraight
function maybeWheel
function checkGroups
function evalGroupedRanks
function groupBySuccCards
function sortByLength
-}



-- bug found in maybeFlush: if running with aceLowStraight -> Gives cards in wrong order

------------------------- maybeFlush --------------------------------

hUnitMaybeFlush :: Bool
hUnitMaybeFlush = and [maybeFlush1,
                       maybeFlush2,
                       maybeFlush3,
                       maybeFlush4]


sameSuitFlushProp :: Property
sameSuitFlushProp = forAll genListHand $ \hand ->
         case maybeFlush hand of
                Nothing    -> True
                Just cards -> all (==head suits) suits
                 where
                  suits = map suit cards


-- Flush
maybeFlush1 :: Bool
maybeFlush1 = Just [Card Queen Clubs,
                    Card Jack Clubs,
                    Card Ten Clubs,
                    Card Eight Clubs,
                    Card Five Clubs] == handData
        where
            hand = [Card Four Hearts,
                    Card Five Clubs,                    
                    Card Eight Clubs,    
                    Card Ten Clubs,
                    Card Jack Clubs,
                    Card Queen Clubs,
                    Card Ace Diamonds ]
            handData = maybeFlush hand


-- Not Flush (HighCard)
maybeFlush2 :: Bool
maybeFlush2 = Nothing == handData
        where
            hand = [Card Four Hearts,
                    Card Five Clubs,                    
                    Card Eight Hearts,    
                    Card Ten Clubs,
                    Card Jack Hearts,
                    Card Queen Clubs,
                    Card Ace Diamonds ]
            handData = maybeFlush hand



-- same suit on all card should keep all cards, otherwise we could miss potential starightFlushes
maybeFlush3 :: Bool
maybeFlush3 = (Just $ reverse hand)  == handData
        where
            hand = [Card Four Clubs,
                    Card Five Clubs,                    
                    Card Eight Clubs,    
                    Card Ten Clubs,
                    Card Jack Clubs,
                    Card Queen Clubs,
                    Card Ace Clubs ]
            handData = maybeFlush hand


-- wheel-flush (should put ace last in list )
maybeFlush4 :: Bool
maybeFlush4 = Just [Card Five Hearts,
                    Card Four Hearts,
                    Card Three Hearts,
                    Card Two Hearts,
                    Card Ace Hearts]  == handData
        where
            hand = [Card Two Hearts,
                    Card Three Hearts,
                    Card Four Hearts,
                    Card Five Hearts,
                    Card Ace Hearts]
            handData = maybeFlush hand


------------------------- maybeStraight--------------------------------

lengthStraightprop :: Property
lengthStraightprop = forAll genListHand $ \hand ->
         case maybeStraight hand of
                Nothing    -> True
                Just cards -> length cards == 5

hUnitMaybeStraight :: Bool
hUnitMaybeStraight = and [maybeStraight1,
                          maybeStraight2,
                          maybeStraight3,
                          maybeStraight4,
                          maybeStraight5,
                          maybeStraight6,
                          maybeStraight7]


-- straight
maybeStraight1 :: Bool
maybeStraight1 = Just [ Card Nine Clubs,
                        Card Eight Clubs,                    
                        Card Seven Clubs,                    
                        Card Six Hearts ,                    
                        Card Five Hearts] == handData
        where
            hand = [Card Three Hearts,
                    Card Five Hearts,
                    Card Six Hearts,
                    Card Seven Clubs,
                    Card Eight Clubs,
                    Card Nine Clubs,
                    Card Ace Spades]
            handData = maybeStraight hand

-- Royal straight
maybeStraight2 :: Bool
maybeStraight2 = Just [ Card Ace Clubs,
                         Card King Hearts,                    
                         Card Queen Clubs,                    
                         Card Jack Clubs,                    
                         Card Ten Hearts] == handData
        where
            hand = [Card Three Hearts,
                    Card Five Hearts,
                    Card Six Hearts,
                    Card Ten Hearts,
                    Card Jack Clubs,   
                    Card Queen Clubs,   
                    Card King Hearts, 
                    Card Ace Clubs]
            handData = maybeStraight hand

-- wheel-straight
maybeStraight3 :: Bool
maybeStraight3 = Just [Card Five Clubs,
                       Card Four Clubs,
                       Card Three Hearts,
                       Card Two Hearts,                    
                       Card Ace Hearts] == handData
        where
            hand = [Card Two Hearts,
                    Card Three Hearts,
                    Card Four Clubs,
                    Card Five Clubs,
                    Card Ace Hearts]
            handData = maybeStraight hand

-- no straight
maybeStraight4 :: Bool
maybeStraight4 = Nothing == handData
        where
            hand = [Card Two Hearts,
                    Card Three Hearts,
                    Card Seven Clubs,
                    Card Ten Clubs,
                    Card Jack Hearts,
                    Card King Clubs,
                    Card Ace Hearts]
            handData = maybeStraight hand


-- long straight [2,3,4,5,6,7,8]
maybeStraight5 :: Bool
maybeStraight5 = Just [Card Eight Spades,                    
                       Card Seven Diamonds,
                       Card Six Clubs,                                          
                       Card Five Hearts, 
                       Card Four Clubs ] == handData
        where
           hand = [ Card Two Clubs, 
                    Card Three Diamonds, 
                    Card Four Clubs, 
                    Card Five Hearts, 
                    Card Six Clubs, 
                    Card Seven Diamonds, 
                    Card Eight Spades]
           handData = maybeStraight hand



-- long straight with wheel [2,3,4,5,6,7,8,A]
maybeStraight6 :: Bool
maybeStraight6 = Just [Card Eight Spades,                    
                       Card Seven Diamonds,
                       Card Six Clubs,                                          
                       Card Five Hearts, 
                       Card Four Clubs ] == handData
        where
           hand = [ Card Two Clubs, 
                    Card Three Diamonds, 
                    Card Four Clubs, 
                    Card Five Hearts, 
                    Card Six Clubs, 
                    Card Seven Diamonds, 
                    Card Eight Spades,
                    Card Ace Spades]
           handData = maybeStraight hand


-- straight with duplicates
maybeStraight7 :: Bool
maybeStraight7 = Just [ Card Nine Clubs,
                        Card Eight Clubs,                    
                        Card Seven Diamonds,                    
                        Card Six Hearts ,                    
                        Card Five Hearts] == handData
        where
            hand = [Card Three Hearts,
                    Card Five Hearts,
                    Card Six Hearts,
                    Card Seven Diamonds,
                    Card Seven Clubs,
                    Card Eight Clubs,
                    Card Nine Clubs]
            handData = maybeStraight hand


------------------------- maybeWheel--------------------------------

hUnitmaybeWheel :: Bool
hUnitmaybeWheel = and [maybeWheel1,
                       maybeWheel2,
                       maybeWheel3,
                       maybeWheel4,
                       maybeWheel5]

-- wheel 
maybeWheel1 :: Bool
maybeWheel1 = Just [Card Five Clubs,
                    Card Four Clubs,
                    Card Three Hearts,
                    Card Two Hearts,                    
                    Card Ace Hearts] == handData
        where
            hand = [Card Two Hearts,
                    Card Three Hearts,
                    Card Four Clubs,
                    Card Five Clubs,
                    Card Ace Hearts]
            handData = maybeWheel hand

-- wheel med avbrott
maybeWheel2 :: Bool
maybeWheel2 = Just [Card Five Clubs,
                    Card Four Clubs,
                    Card Three Hearts,
                    Card Two Hearts,                    
                    Card Ace Hearts] == handData
        where
            hand = [Card Two Hearts,
                    Card Three Hearts,
                    Card Four Clubs,
                    Card Five Clubs,
                    Card Eight Spades,
                    Card Ten Spades,
                    Card Ace Hearts]
            handData = maybeWheel hand

-- not wheel
maybeWheel3 :: Bool
maybeWheel3 = Nothing == handData
        where
            hand = [Card Two Hearts,
                    Card Three Hearts,
                    Card Five Clubs,
                    Card Eight Spades,
                    Card Ten Spades,
                    Card Ace Hearts]
            handData = maybeWheel hand


-- Straight that not is wheel

maybeWheel4 :: Bool
maybeWheel4 = Nothing == handData
        where
            hand = [Card Two Hearts,
                    Card Three Hearts,
                    Card Four Clubs,
                    Card Five Clubs,
                    Card Six Clubs,
                    Card Eight Spades,
                    Card Ten Spades]
            handData = maybeWheel hand

-- less than 5 cards
maybeWheel5 :: Bool
maybeWheel5 = Nothing == handData
        where
            hand = [Card Two Hearts,
                    Card Three Hearts,
                    Card Four Clubs,
                    Card Ace Hearts]
            handData = maybeWheel hand


-------------------------checkGroups--------------------------------


lengthCheckGroupsProp :: Property
lengthCheckGroupsProp =   forAll genListHand $ \hand -> (length . snd . checkGroups) hand == 5


genListHand :: Gen [Card]
genListHand = sort <$> (vectorOf 7 arbitrary)



hUnitCheckGroups :: Bool
hUnitCheckGroups = and [checkGroupsQuads1,
                        checkGroupsQuads2,
                        checkGroupsQuads3,
                        checkGroupsFullHouse1,
                        checkGroupsFullHouse2,
                        checkGroupsFullHouse3,
                        checkGroupsTrips1,
                        checkGroupsTrips2,
                        checkGroupsTrips3,
                        checkGroupsTwos1,
                        checkGroupsTwos2,
                        checkGroupsTwos3,
                        checkGroupsPair1,
                        checkGroupsPair2,
                        checkGroupsPair3,
                        checkGroupsHC1,
                        checkGroupsHC2,
                        checkGroupsHC3 ]



-- | quads
checkGroupsQuads1 :: Bool
checkGroupsQuads1 = [Card Eight Hearts,
                    Card Eight Spades,
                    Card Eight Diamonds,
                    Card Eight Clubs,
                    Card Ten Clubs] == cards
    where
        hand          = [Card Seven Hearts,
                         Card Eight Hearts,
                         Card Eight Clubs,
                         Card Eight Diamonds,
                         Card Eight Spades,
                         Card Nine Hearts,
                         Card Ten Clubs] 
        (comb, cards) = checkGroups hand

-- | quads and threeOfAKind [4,3]
checkGroupsQuads2 :: Bool
checkGroupsQuads2 = [Card Eight Hearts, 
                     Card Eight Spades, 
                     Card Eight Diamonds, 
                     Card Eight Clubs , 
                     Card Ten Hearts] == cards
    where
        hand          = [Card Eight Hearts,
                         Card Eight Spades, 
                         Card Eight Diamonds, 
                         Card Eight Clubs , 
                         Card Ten Hearts, 
                         Card Ten Spades, 
                         Card Ten Diamonds]
        (comb, cards) = checkGroups hand

-- | quads and threeOfAKind [3, 4]
checkGroupsQuads3 :: Bool
checkGroupsQuads3 = [Card Ten Hearts, 
                     Card Ten Spades, 
                     Card Ten Diamonds, 
                     Card Ten Clubs , 
                     Card Eight Spades] == cards
    where
        hand          = [Card Eight Spades, 
                         Card Eight Diamonds, 
                         Card Eight Clubs , 
                         Card Ten Hearts, 
                         Card Ten Spades, 
                         Card Ten Diamonds, 
                         Card Ten Clubs]
        (comb, cards) = checkGroups hand


-- | fullHouse [3,2]
checkGroupsFullHouse1 :: Bool
checkGroupsFullHouse1 = [Card Five Hearts, 
                        Card Five Spades, 
                        Card Five Diamonds,
                        Card Six Hearts, 
                        Card Six Diamonds ] == cards
        where
            hand = [Card Five Hearts, 
                    Card Five Spades, 
                    Card Five Diamonds, 
                    Card Six Hearts, 
                    Card Six Diamonds, 
                    Card Eight Hearts, 
                    Card Ten Hearts]
            (comb, cards) = checkGroups hand

-- | fullHouse [2,3]
checkGroupsFullHouse2 :: Bool
checkGroupsFullHouse2 = [Card Six Hearts, 
                        Card Six Diamonds,
                        Card Six Clubs,
                        Card Five Spades, 
                        Card Five Diamonds  ] == cards
        where
            hand = [Card Five Spades, 
                    Card Five Diamonds, 
                    Card Six Hearts, 
                    Card Six Diamonds,
                    Card Six Clubs, 
                    Card Eight Hearts, 
                    Card Ten Hearts]
            (comb, cards) = checkGroups hand

-- | fullHouse [3,3]
checkGroupsFullHouse3 :: Bool
checkGroupsFullHouse3 = [Card Six Hearts, 
                         Card Six Diamonds,
                         Card Six Clubs,
                         Card Five Hearts, 
                         Card Five Spades  ] == cards
        where
            hand = [Card Five Hearts,
                    Card Five Spades, 
                    Card Five Diamonds, 
                    Card Six Hearts, 
                    Card Six Diamonds,
                    Card Six Clubs, 
                    Card Eight Hearts, 
                    Card Ten Hearts]
            (comb, cards) = checkGroups hand



-- threeOfAKind [3]
checkGroupsTrips1 :: Bool
checkGroupsTrips1 = [Card Eight Hearts, 
                    Card Eight Diamonds, 
                    Card Eight Clubs,
                    Card Ace Hearts,
                    Card Ten Clubs] == cards
        where
            hand = [Card Two Hearts,
                    Card Three Spades, 
                    Card Eight Diamonds, 
                    Card Eight Hearts, 
                    Card Eight Clubs,
                    Card Ten Clubs, 
                    Card Ace Hearts]
            (comb, cards) = checkGroups hand

-- threeOfAKind [3] & Flush (it should give ThreeOfAKind)
checkGroupsTrips2 :: Bool
checkGroupsTrips2 = (ThreeOfAKind, [Card Eight Hearts, 
                                    Card Eight Diamonds, 
                                    Card Eight Clubs,
                                    Card Ace Clubs,
                                    Card Jack Clubs]) == handData
        where
            hand = [Card Eight Diamonds, 
                    Card Eight Hearts, 
                    Card Eight Clubs,
                    Card Nine Clubs,
                    Card Jack Clubs,
                    Card Ace Clubs]
            handData = checkGroups hand


-- threeOfAKind [3] & Straight (it should give ThreeOfAKind)
checkGroupsTrips3 :: Bool
checkGroupsTrips3 = (ThreeOfAKind, [Card Eight Hearts, 
                                    Card Eight Diamonds, 
                                    Card Eight Clubs,
                                    Card Queen Diamonds,
                                    Card Jack Clubs]) == handData
        where
            hand = [Card Eight Diamonds, 
                    Card Eight Hearts, 
                    Card Eight Clubs,
                    Card Nine Clubs,
                    Card Ten Hearts,
                    Card Jack Clubs,
                    Card Queen Diamonds]
            handData = checkGroups hand


--TwoPairs
checkGroupsTwos1 :: Bool
checkGroupsTwos1 = (TwoPairs, [Card King Hearts,
                              Card King Clubs,
                              Card Jack Hearts, 
                              Card Jack Diamonds, 
                              Card Ace Diamonds]) == handData
        where
            hand = [Card Three Diamonds, 
                    Card Four Hearts, 
                    Card Jack Hearts,
                    Card Jack Diamonds,
                    Card King Hearts,
                    Card King Clubs,
                    Card Ace Diamonds]
            handData = checkGroups hand

--TwoPairs & Flush (should give TwoPairs)
checkGroupsTwos2 :: Bool
checkGroupsTwos2 = (TwoPairs, [Card King Hearts,
                              Card King Clubs,
                              Card Jack Hearts, 
                              Card Jack Diamonds, 
                              Card Ace Hearts]) == handData
        where
            hand = [Card Three Hearts, 
                    Card Four Hearts, 
                    Card Jack Hearts,
                    Card Jack Diamonds,
                    Card King Hearts,
                    Card King Clubs,
                    Card Ace Hearts]
            handData = checkGroups hand

--TwoPairs & Straight (should give TwoPairs)
checkGroupsTwos3 :: Bool
checkGroupsTwos3 = (TwoPairs, [Card Six Hearts,
                               Card Six Clubs,
                               Card Five Diamonds,
                               Card Five Clubs,
                               Card Seven Hearts]) == handData
        where
            hand = [Card Three Hearts, 
                    Card Four Hearts, 
                    Card Five Diamonds,
                    Card Five Clubs,
                    Card Six Hearts,
                    Card Six Clubs,
                    Card Seven Hearts]
            handData = checkGroups hand





-- Pair
checkGroupsPair1 :: Bool 
checkGroupsPair1 = (Pair, [Card Jack Spades,
                          Card Jack Clubs,
                          Card Ace Clubs,
                          Card Eight Spades,
                          Card Seven Spades]) == handData
      where                    
            hand = [Card Two Hearts,
                    Card Six Hearts,
                    Card Seven Spades,
                    Card Eight Spades,
                    Card Jack Spades,
                    Card Jack Clubs,
                    Card Ace Clubs 

                    ]
            handData = checkGroups hand



-- Pair & Flush (should give Pair)
checkGroupsPair2 :: Bool 
checkGroupsPair2 = (Pair, [Card Jack Hearts,
                          Card Jack Clubs,
                          Card Ace Hearts,
                          Card Eight Spades,
                          Card Seven Hearts]) == handData
      where                    
            hand = [Card Two Hearts,
                    Card Six Hearts,
                    Card Seven Hearts,
                    Card Eight Spades,
                    Card Jack Hearts,
                    Card Jack Clubs,
                    Card Ace Hearts 

                    ]
            handData = checkGroups hand


-- Pair & Straight (Should give Pair)
checkGroupsPair3 :: Bool 
checkGroupsPair3 = (Pair, [Card Six Hearts,
                           Card Six Clubs,
                           Card Ace Hearts,
                           Card Five Clubs,
                           Card Four Spades]) == handData
      where                    
            hand = [Card Two Hearts,
                    Card Three Hearts,
                    Card Four Spades,
                    Card Five Clubs,
                    Card Six Hearts,
                    Card Six Clubs,
                    Card Ace Hearts 

                    ]
            handData = checkGroups hand



--HighCard
checkGroupsHC1 :: Bool 
checkGroupsHC1 = (HighCard, take 5 $ reverse hand) == handData
      where                    
            hand = [Card Two Hearts,
                    Card Five Hearts,
                    Card Eight Spades,
                    Card Jack Clubs,
                    Card Queen Hearts,
                    Card King Clubs,
                    Card Ace Hearts 
                    ]
            handData = checkGroups hand


--HighCard & Flush (should give HighCard)
checkGroupsHC2 :: Bool 
checkGroupsHC2 = (HighCard, take 5 $ reverse hand) == handData
      where                    
            hand = [Card Two Hearts,
                    Card Five Hearts,
                    Card Eight Hearts,
                    Card Jack Hearts,
                    Card Queen Hearts,
                    Card King Hearts,
                    Card Ace Hearts 
                    ]
            handData = checkGroups hand


--HighCard & Straight (should give HighCard)
checkGroupsHC3 :: Bool 
checkGroupsHC3 = (HighCard, take 5 $ reverse hand) == handData
      where                    
            hand = [Card Two Hearts,
                    Card Three Hearts,
                    Card Four Spades,
                    Card Five Clubs,
                    Card Six Hearts,
                    Card King Clubs,
                    Card Ace Hearts 
                    ]
            handData = checkGroups hand



----------------------evalGroupedRanks------------------------------

evalGroupedRanksProp :: Property
evalGroupedRanksProp = forAll genIntList evalGroupedRanksPropHelp


evalGroupedRanksPropHelp :: [RankGroup] ->  Bool
evalGroupedRanksPropHelp rankGroup
    | (take 1 sorted == [4]  ) && (combination == Quads)        = True
    | (take 2 sorted == [3,2]) && (combination == FullHouse)    = True
    | (take 2 sorted == [3,3]) && (combination == FullHouse)    = True
    | (take 2 sorted == [3,3]) && (combination == FullHouse)    = True
    | (take 1 sorted == [3]  ) && (combination == ThreeOfAKind) = True
    | (take 2 sorted == [2,2]) && (combination == TwoPairs)     = True
    | (take 1 sorted == [2]  ) && (combination == Pair    )     = True
    | (sorted /= []          ) && (combination == HighCard)     = True
    | otherwise = False
    where
        sorted = reverse $ sort $ rankGroup
        combination = evalGroupedRanks sorted


genIntList :: Gen [RankGroup]
genIntList =  vectorOf 7 (choose (1,4))





----------------------groupBySuccCards------------------------------

hUnitGroupBySucc :: Bool
hUnitGroupBySucc = and [groupBySuccCardsTest1,
                        groupBySuccCardsTest2,
                        groupBySuccCardsTest3,
                        groupBySuccCardsTest4,
                        groupBySuccCardsTest5]

groupBySuccCardsTest1 :: Bool
groupBySuccCardsTest1 = groupBySuccCards [Card King Hearts, Card Ace Hearts] ==  [[Card King Hearts, Card Ace Hearts]]

groupBySuccCardsTest2 :: Bool
groupBySuccCardsTest2  = groupBySuccCards [Card Jack Hearts, Card Ace Hearts] ==  [[Card Jack Hearts] , [Card Ace Hearts]]


groupBySuccCardsTest3 :: Bool
groupBySuccCardsTest3 = groupBySuccCards [Card Five Hearts, Card Six Hearts, Card Jack Hearts, Card Queen Hearts] ==  [[Card Five Hearts, Card Six Hearts], [Card Jack Hearts, Card Queen Hearts]]

groupBySuccCardsTest4 :: Bool
groupBySuccCardsTest4 = groupBySuccCards [ Card Two Clubs, Card Three Diamonds, Card Four Clubs, Card Five Hearts, Card Ace Diamonds] == [ [Card Two Clubs, Card Three Diamonds, Card Four Clubs, Card Five Hearts] , [Card Ace Diamonds]]


groupBySuccCardsTest5 :: Bool
groupBySuccCardsTest5 =  groupBySuccCards [Card Jack Clubs, Card Jack Spades, Card King Hearts, Card King Spades, Card King Hearts, Card Ace Clubs] == [[Card Jack Clubs],[Card Jack Spades],[Card King Hearts],[Card King Spades],[Card King Hearts, Card Ace Clubs]]


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
-----------------------------------------------------------------
-----------------------------------------------------------------

                --- Manual testing ---

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
hand29 = [ Card Two Clubs, Card Three Diamonds, Card Four Clubs, Card Five Hearts, Card Ace Diamonds]

hand30 :: Hand -- Straight Ace Low
hand30 = [ Card Two Diamonds, Card Three Diamonds, Card Four Diamonds, Card Five Diamonds, Card Ace Diamonds]




hand31 :: Hand
hand31 = [Card Two Hearts, Card Three Spades, Card Four Hearts, Card Five Clubs]

hand32 :: Hand -- Straight Ace Low
hand32 = [ Card Two Clubs, Card Three Diamonds, Card Four Clubs, Card Five Hearts, Card King Hearts, Card Ace Diamonds]

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
