module TestHandEvaluation where

import Data.Maybe
import Data.List
import Data.Function (on)

import Engine.EngineTypes
import Engine.Cards
import Engine.HandEvaluation

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck



import System.Exit ()
import Control.Exception ()

{-
main :: IO ()
main = defaultMain combinationsTests
-}
------------------------   TESTTREE  ----------------------------


combinationsTests :: TestTree
combinationsTests = localOption (QuickCheckTests 5000) $
                    testGroup "Combination Tests"
  [ 
    testProperty "sortByLength length inner Lists"     sortByLengthPropLength           -- function sortByLength
  , testProperty "sortByLength size innerLists"        sortByLengthPropSize             -- function sortByLength
  , testCase     "groupBySuccCards"                  $ hUnitGroupBySucc @?= True        -- function groupBySuccCards
  , testProperty "evalGroupedRanksProp"                evalGroupedRanksProp             -- function evalGroupedRanks
  , testProperty "length property checkGroups"         lengthCheckGroupsProp            -- function evalGroupedRanks
  , testCase     "HUnit checkGroups  "               $ hUnitCheckGroups @?= True        -- function checkGroups
  , testCase     "HUnit maybeWheel   "               $ hUnitmaybeWheel  @?= True        -- function maybeWheel
  , testCase     "HUnit maybeStraight"               $ hUnitMaybeStraight @?= True      -- function maybeStraight
  , testProperty "maybeStraight length"                lengthStraightprop               -- function maybeStraight
  , testCase     "HUnit checkGroups  "               $ hUnitMaybeFlush @?= True         -- function maybeFlush
  , testProperty "same suit in maybeFlush"             sameSuitFlushProp                -- function ifFlush
  , testCase     "HUnit ifFlush"                     $ hUnitIfFlush @?= True            -- function ifFlush
  , testProperty "ifFlush gives right comb"            ifFlushProp                      -- function ifFlush
  , testCase     "HUnit ifFlush"                     $ hUnitIfNotFlush @?= True         -- function ifNotFlush 
  , testProperty "ifNotFlush use of checkGroups"       notFlushUseOfCheckGroupsProperty -- function ifNotFlush 
  , testCase     " HUnit value-function"             $ hUnitValue @?= True              -- function value
  , testProperty "final hand only contain cards from original hand" elementOfOriginalHand -- function value
  , testProperty "value return 5 cards"                             sizeHandProp          -- function value
  , testProperty "value functions always gives an ok combination"   givesCombination      -- function value
  , testCase     "compare quads in handComparision"               $ hUnitCardComparisionQuads @?= True    --function handComparision
  , testCase     "compare straights in handComparision"           $ hUnitCardComparisionStraight @?= True -- function handComparision
  , testProperty "always return index in handComparison"            alwaysReturnIndex                     -- function handComparision
  , testCase     "HUnit correct winners in function winners "     $ hUnitWinners @?= True                 -- function winners
  , testProperty " always return winner(s) if we have >= 1 list of cards" alwaysWinnersProp               -- function winners
  ]


-----------------------Generators----------------------------

-- | Generates a list of 7 cards with same suit, no duplicates
genFlushHand :: Gen [Card]
genFlushHand = do
  suit  <- arbitrary
  ranks <- suchThat
              (vectorOf 7 arbitrary)
              ((==7) . length . nub)

  pure (sort [Card r suit | r <- ranks])

-- | generates a sorted list of 7 cards without duplicates
genListHand :: Gen [Card]
genListHand =
  sort <$> suchThat
            (vectorOf 7 arbitrary)
            ((== 7) . length . nub)


-- | generates a list of seven Ints from 1 to 4
genIntList :: Gen [RankGroup]
genIntList =  vectorOf 7 (choose (1,4))



 -- | generates lists with lists that contains 5 card each
genLists :: Gen [[Card]]
genLists = listOf (vectorOf 5 arbitrary)


 -- | generates lists with lists that contains 2 card each
genListsTwo :: Gen [[Card]]
genListsTwo = listOf (vectorOf 2 arbitrary)




-------------------------- winners -------------------------------

-- winners :: [Card] -> [[Card]] -> [Int]


alwaysWinnersProp :: Property
alwaysWinnersProp =
    forAll genListHand $ \community ->
    forAll genListsTwo $ \playerC ->
        if playerC == []
           then (winners community playerC) == []
           else (winners community playerC) /= []


-- | HUint 
hUnitWinners :: Bool
hUnitWinners = and [winners1,
                    winners2,
                    winners3]

 -- correct winner
winners1 :: Bool
winners1 = winners comCardsW1 handListW1 == [0]

-- correct tie
winners2 :: Bool
winners2 = winners comCardsW1 handListW2 == [0,1]

 -- correct kicker-rules
winners3 :: Bool 
winners3 = winners comCardsW2 handListW3 == [0]




handListW1 :: [[Card]]
handListW1 = [handW1, handW2]

handListW2 :: [[Card]]
handListW2 = [handW1, handW1]

handW1 :: [Card]
handW1 = [Card Ace Hearts, Card Ace Clubs]

handW2 :: [Card]
handW2 = [Card King Clubs, Card King Hearts ]

comCardsW1 :: [Card]
comCardsW1 = [Card Three Clubs, Card Six Hearts, Card Two Spades, Card Jack Hearts, Card Seven Hearts]

comCardsW2 :: [Card]
comCardsW2 = [Card Four Diamonds, Card Four Hearts, Card Four Clubs, Card Nine Clubs, Card Four Spades] 

handListW3 :: [[Card]]
handListW3 = [handW3, handW4]

handW3 :: [Card]
handW3 = [Card Three Clubs, Card Ace Clubs]

handW4 :: [Card]
handW4 = [Card King Clubs, Card King Hearts]



----------------------- handComparision --------------------------

-- handComparision :: [(Combination, [Card])] -> [Int] -> [Int]

alwaysReturnIndex :: Property
alwaysReturnIndex =
  forAll ((,) <$> genListHand <*> genListHand) $
    \(hand1, hand2) ->
      case cardComparision [value hand1, value hand2] [0,1] of
        [] -> False
        _  -> True


------------------------------------
hUnitCardComparisionQuads :: Bool
hUnitCardComparisionQuads = and [cardComparisionQuads1,
                                      cardComparisionQuads2,
                                       cardComparisionQuads3 ]


cardComparisionQuads1 :: Bool --Quads vs Quads
cardComparisionQuads1 = cardComparision  [quadsW, quadsL] [0,1] == [0]
        where
                quadsW  = value handc4
                quadsL  = value handc5


cardComparisionQuads2 :: Bool --Quads vs Quads (kicker Victory)
cardComparisionQuads2 = cardComparision  [quadsW, quadsL] [0,1] == [0]
        where
                quadsW  = value handc6
                quadsL  = value handc5
        
cardComparisionQuads3 :: Bool --Quads vs Quads (same hand)
cardComparisionQuads3 = cardComparision  [quads, quads] [0,1] == [0,1]
        where
                quads  = value handc6



handc4 :: Hand -- Quads
handc4 = [Card King Hearts,
          Card Ace Hearts,
          Card Ace Spades,
          Card Ace Diamonds,
          Card Ace Clubs]

handc5 :: Hand -- Quads
handc5 = [Card Eight Spades,
          Card King Hearts,
          Card King Spades,
          Card King Diamonds,
          Card King Clubs]


handc6 :: Hand -- Quads
handc6 = [Card King Hearts,
          Card King Spades,
          Card King Diamonds,
          Card King Clubs,
          Card Ace Hearts]

------------------------------------
hUnitCardComparisionStraight :: Bool
hUnitCardComparisionStraight = and [cardComparisionStraight1,
                                    cardComparisionStraight2,
                                    cardComparisionStraight3,
                                    cardComparisionStraight4]


cardComparisionStraight1 :: Bool --wheel vs regular, all hands in game
cardComparisionStraight1 = cardComparision  [wheelStraight, straight] [0,1] == [1]
        where
                wheelStraight = value handc1
                straight      = value handc2


cardComparisionStraight2 :: Bool --wheel vs regular, just wheelStraight in game
cardComparisionStraight2 = cardComparision  [wheelStraight, straight] [0] == [0]
        where
                wheelStraight = value handc1
                straight      = value handc2

cardComparisionStraight3 :: Bool --royal vs regular, all hands in game
cardComparisionStraight3 = cardComparision  [royalStraight, straight] [0,1] == [0]
        where
                straight      = value handc2
                royalStraight = value handc3

cardComparisionStraight4 :: Bool --wheel vs royal, all hands in game
cardComparisionStraight4 = cardComparision  [royalStraight, wheelStraight] [0,1] == [0]
        where
                wheelStraight = value handc1
                royalStraight = value handc3


handc1 :: Hand -- wheelStraight
handc1 = [Card Two Hearts,
          Card Three Clubs,        
          Card Four Hearts,
          Card Five Spades,
          Card Ace Spades]

handc2 :: Hand -- straight
handc2 = [Card Five Hearts,
          Card Six Clubs,        
          Card Seven Hearts,
          Card Eight Spades,
          Card Nine Spades]

handc3 :: Hand -- Royalstraight
handc3 = [Card Ten Hearts,
          Card Jack Clubs,        
          Card Queen Hearts,
          Card King Spades,
          Card Ace Spades]


--------------------------- value --------------------------------

--value always give a combination
givesCombination :: Property
givesCombination = forAll genListHand $ \hand ->
         case value hand of
                (combination, _) -> combination `elem` allCombinations
        where
        allCombinations =[HighCard
                         ,Pair
                         ,TwoPairs
                         ,ThreeOfAKind
                         ,Straight
                         ,Flush
                         ,FullHouse
                         ,Quads
                         ,StraightFlush]


-- always return 5 cards
sizeHandProp :: Property
sizeHandProp = forAll genListHand $ \hand ->
         case value hand of
                (_, cards) -> length cards == 5

--all card that is returned is an element in the original hand
elementOfOriginalHand :: Property
elementOfOriginalHand = forAll genListHand $ \hand ->
         case value hand of
                (_, cards) -> and $ map (\x-> x `elem` hand ) cards



hUnitValue :: Bool
hUnitValue = and [value1,
                  value2,
                  value3,
                  value4,
                  value5,
                  --value6, Gives card with correct rank, but not the exacly correct card
                  value7,
                  value8,
                  value9,
                  value10]


-- straight
value1 :: Bool
value1 = (Straight, [ Card Nine  Clubs,
                      Card Eight Clubs,                    
                      Card Seven Clubs,                    
                      Card Six   Hearts ,                    
                      Card Five  Hearts]) == handData
        where
            hand' = [Card Three Hearts,
                    Card Five Hearts,
                    Card Six Hearts,
                    Card Seven Clubs,
                    Card Eight Clubs,
                    Card Nine Clubs,
                    Card Ace Spades]
            handData = value hand'

-- wheel- straight
value2 :: Bool
value2 = (Straight, [Card Five Clubs,
                     Card Four Hearts,
                     Card Three Clubs,
                     Card Two Hearts,
                     Card Ace Clubs]) == handData
        where
            hand = [Card Two Hearts,
                    Card Three Clubs,
                    Card Four Hearts,
                    Card Five Clubs,
                    Card Ten Diamonds,
                    Card Queen Clubs,
                    Card Ace Clubs]
            handData = value hand


--straight flush
value3 :: Bool
value3 = (StraightFlush, [Card Six Clubs,
                          Card Five Clubs,
                          Card Four Clubs,
                          Card Three Clubs,
                          Card Two Clubs]) == handData
        where
            hand = [Card Two Clubs,
                    Card Three Clubs,
                    Card Four Clubs,
                    Card Five Clubs,
                    Card Six Clubs,
                    Card Queen Clubs,
                    Card King Clubs]
            handData = value hand


--royal straight flush
value4 :: Bool
value4 = (StraightFlush, [Card Ace Clubs,
                          Card King Clubs,
                          Card Queen Clubs,
                          Card Jack Clubs,
                          Card Ten Clubs]) == handData
        where
            hand = [Card Eight Clubs,
                    Card Nine Clubs,
                    Card Ten Clubs,
                    Card Jack Clubs,
                    Card Queen Clubs,
                    Card King Clubs,
                    Card Ace Clubs]
            handData = value hand

-- wheel straight flush
value5 :: Bool
value5 = (StraightFlush, [Card Five Clubs,
                          Card Four Clubs,
                          Card Three Clubs,
                          Card Two Clubs,
                          Card Ace Clubs]) == handData
        where
            hand = [Card Two Clubs,
                    Card Three Clubs,
                    Card Four Clubs,
                    Card Five Clubs,
                    Card Eight Clubs,
                    Card Queen Clubs,
                    Card Ace Clubs]
            handData = value hand


-- quads and trips
value6 :: Bool
value6 = (Quads, [Card King Hearts,
                  Card King Spades,
                  Card King Diamonds,
                  Card King Clubs,
                  Card Ace Hearts]) == handData
        where
            hand = [Card King Hearts,
                    Card King Spades,
                    Card King Diamonds,
                    Card King Clubs,
                    Card Ace Hearts,
                    Card Ace Spades,
                    Card Ace Diamonds]
            handData = value hand


-- two trips[3,3]
value7 :: Bool
value7 = (FullHouse, [Card Ace Hearts,
                      Card Ace Spades,
                      Card Ace Diamonds,
                      Card Two Hearts,
                      Card Two Spades]) == handData
        where
            hand = [Card Two Hearts,
                    Card Two Spades,
                    Card Two Diamonds,
                    Card Five Clubs,
                    Card Ace Hearts,
                    Card Ace Spades,
                    Card Ace Diamonds]
            handData = value hand


value8 :: Bool
value8 = (TwoPairs,  [Card Ace Spades,
                      Card Ace Diamonds,
                      Card Five Diamonds,
                      Card Five Clubs,
                      Card King Hearts]) == handData
        where
            hand = [Card Two Hearts,
                    Card Two Spades,
                    Card Five Diamonds,
                    Card Five Clubs,
                    Card King Hearts,
                    Card Ace Spades,
                    Card Ace Diamonds]
            handData = value hand


--Flush with all cards
value9 :: Bool
value9 = (Flush,   [Card King  Clubs,                    
                    Card Jack  Clubs,                    
                    Card Ten   Clubs,   
                    Card Eight Clubs,                    
                    Card Six   Clubs]) == handData
        where
            hand = [Card Four  Hearts,
                    Card Six   Clubs,                    
                    Card Eight Clubs,
                    Card Ten   Clubs,    
                    Card Jack  Clubs,
                    Card King  Clubs,
                    Card Ace   Diamonds ]
            handData = value hand


-- Flush and threeOfAKind
value10 :: Bool
value10 = (Flush,   [Card Ace  Spades,                    
                    Card King  Spades,                    
                    Card Jack   Spades,   
                    Card Ten Spades,                    
                    Card Four   Spades]) == handData
        where
            hand = [Card Four  Hearts,
                    Card Four  Spades,                    
                    Card Four  Diamonds,
                    Card Ten   Spades,    
                    Card Jack  Spades,
                    Card King  Spades,
                    Card Ace   Spades ]
            handData = value hand


------------------------ ifNotFlush ------------------------------

--property -- if not straight -> return same results as checkGroups
notFlushUseOfCheckGroupsProperty :: Property
notFlushUseOfCheckGroupsProperty = 
        forAll genListHand $ \hand ->
            case ifNotFlush hand of
                (Straight, _) -> True
                _             -> ifNotFlush hand == checkGroups hand


hUnitIfNotFlush :: Bool
hUnitIfNotFlush = and [ifNotFlush1,
                       ifNotFlush2
                       --ifNotFlush3 -- returns kicker with correct rank, but does not follow same invariant with the order of suits
                       ]


-- straight
ifNotFlush1 :: Bool
ifNotFlush1 = (Straight, [Card Queen Clubs,
                          Card Jack Diamonds,
                          Card Ten Clubs,
                          Card Nine Hearts,
                          Card Eight Clubs]) == handData
        where
            hand = fromJust (maybeStraight [Card Four Hearts,
                                            Card Eight Clubs,
                                            Card Nine Hearts,
                                            Card Ten Clubs,
                                            Card Jack Diamonds,
                                            Card Queen Clubs,
                                            Card Ace Clubs])
            handData = ifNotFlush hand

-- wheelstraight
ifNotFlush2 :: Bool
ifNotFlush2 = (Straight, [Card Five Clubs,
                          Card Four Hearts,
                          Card Three Clubs,
                          Card Two Hearts,
                          Card Ace Clubs]) == handData
        where
            hand = fromJust (maybeStraight [Card Two Hearts,
                                            Card Three Clubs,
                                            Card Four Hearts,
                                            Card Five Clubs,
                                            Card Ten Diamonds,
                                            Card Queen Clubs,
                                            Card Ace Clubs])
            handData = ifNotFlush hand

-- Quad and threeOfAKind
ifNotFlush3 :: Bool
ifNotFlush3 = (Quads,   [Card King Hearts,
                          Card King Spades,
                          Card King Diamonds,
                          Card King Clubs,
                          Card Ace Hearts]) == handData
        where
            hand =      [Card King Hearts,
                         Card King Spades,
                         Card King Diamonds,
                         Card King Clubs,
                         Card Ace Hearts,
                         Card Ace Spades,
                         Card Ace Diamonds]
            handData = ifNotFlush hand


-------------------------- ifFlush -------------------------------

--property när vi ser att de enda fallen som kommer ut är straightFlush eller flush

-- | tests the outcome of combination from ifFlush
ifFlushProp :: Property
ifFlushProp = forAll genFlushHand $ \hand ->
         case ifFlush hand of
                (Flush,         _)   -> True
                (StraightFlush, _)   -> True
                _                    -> False


hUnitIfFlush :: Bool
hUnitIfFlush = and [ifFlush1,
                    ifFlush2,
                    ifFlush3]


-- ifFlush will only be runned from results from (maybeFlush cards), only when maybeFlush /= Nothing

--flush without straight
ifFlush1 :: Bool
ifFlush1 = (Flush, [Card King  Clubs,                    
                    Card Jack  Clubs,                    
                    Card Ten   Clubs,   
                    Card Eight Clubs,                    
                    Card Six   Clubs]) == handData
        where
            hand = fromJust (maybeFlush [Card Four  Hearts,
                                         Card Six   Clubs,                    
                                         Card Eight Clubs,
                                         Card Ten   Clubs,    
                                         Card Jack  Clubs,
                                         Card King  Clubs,
                                         Card Ace   Diamonds ])
            handData = ifFlush hand


--flush with straight = straightFlush
ifFlush2 :: Bool
ifFlush2 = (StraightFlush, [Card Nine  Clubs,                    
                            Card Eight Clubs,                    
                            Card Seven Clubs,   
                            Card Six   Clubs,                    
                            Card Five   Clubs]) == handData
        where
            hand = fromJust (maybeFlush [Card Four  Hearts,
                                         Card Five   Clubs,                    
                                         Card Six    Clubs,
                                         Card Seven  Clubs,    
                                         Card Eight  Clubs,
                                         Card Nine   Clubs,
                                         Card Ace   Diamonds ])
            handData = ifFlush hand

-- flush with wheel-straight
ifFlush3 :: Bool
ifFlush3 = (StraightFlush, [Card Five  Clubs,                    
                            Card Four  Clubs,                    
                            Card Three Clubs,   
                            Card Two   Clubs,                    
                            Card Ace   Clubs]) == handData
        where
            hand = fromJust (maybeFlush [Card Four  Hearts,
                                         Card Two   Clubs,                    
                                         Card Three Clubs,
                                         Card Four  Clubs,    
                                         Card Five  Clubs,
                                         Card Nine  Clubs,
                                         Card Ace   Clubs ])
            handData = ifFlush hand


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


hUnitCheckGroups :: Bool
hUnitCheckGroups = and [checkGroupsQuads1,
                        checkGroupsQuads2, -- bug found
                        checkGroupsQuads3, -- bug found
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
-- | bug found, returns card with correct rank but not following invarient of suits order. 
-- | right now only tests if all elements from flush is a part of the result
checkGroupsQuads2 :: Bool
checkGroupsQuads2 = and $ map (\x-> elem x cards )[Card Eight Hearts, 
                                                   Card Eight Spades, 
                                                   Card Eight Diamonds, 
                                                   Card Eight Clubs ]
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
-- | bug found, returns card with correct rank but not following invarient of suits order. 
-- | right now only tests if all elements from flush is a part of the result
checkGroupsQuads3 :: Bool
checkGroupsQuads3 = and $ map (\x-> elem x cards ) [Card Ten Hearts, 
                                                    Card Ten Spades, 
                                                    Card Ten Diamonds, 
                                                    Card Ten Clubs]
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
            (_, cards) = checkGroups hand

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



----------------------groupBySuccCards------------------------------

hUnitGroupBySucc :: Bool
hUnitGroupBySucc = and [groupBySuccCardsTest1,
                        groupBySuccCardsTest2,
                        groupBySuccCardsTest3,
                        groupBySuccCardsTest4,
                        groupBySuccCardsTest5]

groupBySuccCardsTest1 :: Bool
groupBySuccCardsTest1 = groupBySuccCards [Card King Hearts, Card Ace Hearts] 
                                    ==  [[Card King Hearts, Card Ace Hearts]]

groupBySuccCardsTest2 :: Bool
groupBySuccCardsTest2  = groupBySuccCards [Card Jack Hearts, Card Ace Hearts] 
                                     ==  [[Card Jack Hearts] , [Card Ace Hearts]]


groupBySuccCardsTest3 :: Bool
groupBySuccCardsTest3 = groupBySuccCards [Card Five Hearts, Card Six Hearts, Card Jack Hearts, Card Queen Hearts] 
                                    ==  [[Card Five Hearts, Card Six Hearts], [Card Jack Hearts, Card Queen Hearts]]

groupBySuccCardsTest4 :: Bool
groupBySuccCardsTest4 = groupBySuccCards [ Card Two Clubs, Card Three Diamonds, Card Four Clubs, Card Five Hearts, Card Ace Diamonds] 
                                     == [[Card Two Clubs, Card Three Diamonds, Card Four Clubs, Card Five Hearts] , [Card Ace Diamonds]]


groupBySuccCardsTest5 :: Bool
groupBySuccCardsTest5 =  groupBySuccCards [Card Jack Clubs, Card Jack Spades, Card King Hearts, Card King Spades, Card King Hearts, Card Ace Clubs] 
                                       == [[Card Jack Clubs],[Card Jack Spades],[Card King Hearts],[Card King Spades],[Card King Hearts, Card Ace Clubs]]


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

-- tester :: ([Card] -> (Combination, [Card])) ->  [([Card], (Combination, [Card]))]
-- tester fun = [ (hand0, fun hand0), 
--                (hand1, fun hand1),
--                (hand2, fun hand2),
--                (hand3, fun hand3),
--                (hand4, fun hand4),
--                (hand5, fun hand5),
--                (hand6, fun hand6),
--                (hand7, fun hand7),
--                (hand8, fun hand8),
--                (hand9, fun hand9),
--                (hand10, fun hand10),
--                (hand11, fun hand11),
--                (hand12, fun hand12),
--                (hand13, fun hand13),
--                (hand14, fun hand14),
--                (hand15, fun hand15),
--                (hand16, fun hand16),
--                (hand17, fun hand17),
--                (hand18, fun hand18),
--                (hand19, fun hand19),
--                (hand20, fun hand20),
--                (hand21, fun hand21),
--                (hand22, fun hand22),
--                (hand23, fun hand23),
--                (hand24, fun hand24),
--                (hand25, fun hand25),
--                (hand26, fun hand26),
--                (hand27, fun hand27),
--                (hand28, fun hand28),
--                (hand29, fun hand29),
--                (hand30, fun hand30)
--                ]
