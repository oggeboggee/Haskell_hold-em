

module HandEvaluation where

import Cards
import Data.List
import Data.Maybe


----------------------------
-------- Test Hands --------
----------------------------
hand0 :: Hand
hand0 = [Card (Num 4) Hearts, Card (Num 4) Spades ]

hand1 :: Hand
hand1 = [ Card (Num 2) Hearts, Card Jack Spades]

hand2 :: Hand
hand2 = [ Card (Num 2) Hearts, Card Jack Spades, Card (Num 5) Clubs]


hand3 :: Hand
hand3 = [ Card (Num 2) Spades, Card (Num 5) Clubs, Card (Num 2) Clubs, Card Jack Spades ]

-- | sorted
hand4 :: Hand
hand4 = [ Card (Num 2) Spades,  Card (Num 2) Clubs, Card (Num 2) Hearts, Card (Num 5) Clubs, Card Jack Spades ]

hand5 :: Hand
hand5 = [Card (Num 10) Hearts , Card Jack Hearts, Card Queen Hearts, Card King Hearts, Card Ace Hearts]

hand6 :: Hand
hand6 = [Card Queen Hearts, Card Queen Spades, Card King Hearts, Card King Spades, Card Ace Hearts ]

hand7 :: Hand --one pair
hand7 = [Card Jack Hearts, Card Jack Spades, Card Queen Hearts, Card King Spades, Card Ace Hearts ]

hand8 :: Hand --two pair
hand8 = [Card Jack Hearts, Card Jack Spades, Card King Hearts, Card King Spades, Card Ace Hearts]

hand9 :: Hand --two pair
hand9 = [Card Jack Hearts, Card Jack Spades, Card Queen Hearts, Card King Spades, Card King Hearts]

hand10 :: Hand --three of a Kind (x1)
hand10 = [Card Jack Clubs, Card Jack Spades, Card Jack Hearts, Card King Spades, Card King Hearts, Card Ace Clubs]


hand11 :: Hand --three of a Kind (x2)
hand11 = [Card Jack Clubs, Card Jack Spades, Card Jack Hearts, Card King Spades, Card King Hearts, Card King Clubs]



----------------------------
----------------------------

-- | @input Hand
-- | @output list with the cards ranks, sorted
sortRankHandList :: [Card] -> [Rank]
sortRankHandList hand = sort [ rank x | x <- hand]


-- | Bool       = if the combination exist in the hand
-- | Maybe Rank = the Rank of the pair or Nothing
-- | [Int]      =  indexes of the Card that belongs to the pair     




-------------------- Pair --------------------

hasPair :: [Card] -> (Bool, Maybe Rank, Maybe [Int])
hasPair cards
     | hasPairBool cards = (True, Just (rankOfPair cards), Just (pairIndex cards))
     | otherwise   = (False, Nothing, Nothing)


-- | Helpers
hasPairBool :: [Card] -> Bool
hasPairBool cards
     | pairs == 1 = True
     | otherwise  = False
     where
          grHand = group (sortRankHandList cards)        
          mpHand = map (length) grHand
          pairs  = length (filter (==2) mpHand)


rankOfPair :: [Card] -> Rank
rankOfPair cards = rank (cards!!indexOfPair)
         where
          grHand = group (sortRankHandList cards)        
          mpHand = map (length) grHand
          indexOfPair = fromJust (elemIndex 2 mpHand)

pairIndex :: [Card] -> [Int]
pairIndex cards = [indexOfPair, (indexOfPair+1)]
        where
          grHand = group (sortRankHandList cards)        
          mpHand = map (length) grHand
          indexOfPair = fromJust (elemIndex 2 mpHand)

------------------- End Pair --------------------
-------------------- twoPair --------------------

hasTwoPair :: [Card] -> (Bool, Maybe Rank, Maybe [Int])
hasTwoPair cards
     | hasTwoPairBool cards = (True, Just (rankOfHighestPair cards), Just (indexOfPairs cards))
     | otherwise            = (False, Nothing, Nothing)


-- | Helpers
hasTwoPairBool :: [Card] -> Bool
hasTwoPairBool cards
     | pairs == 2 = True
     | pairs == 3 = True
     | otherwise  = False
     where
          grHand = group (sortRankHandList cards)        
          mpHand = map (length) grHand
          pairs  = length (filter (==2) mpHand)


rankOfHighestPair :: [Card] -> Rank
rankOfHighestPair cards = rank (cards!!index)
     where
          grHand = group (sortRankHandList cards)        
          mpHand = map (length) grHand
          index = sum (drop 1 (dropWhile (/= 2) (reverse mpHand))) - 1


indexOfPairs :: [Card] -> [Int]
indexOfPairs cards = [indexFPair, (indexFPair + 1), indexSPair, (indexSPair + 1)]
        where
          grHand     = group (sortRankHandList cards)        
          mpHand     = map (length) grHand
          indexSPair = sum (drop 1 (dropWhile (/=2) (reverse mpHand)))
          indexFPair = sum (drop 1 (dropWhile (/=2) (drop 1 (dropWhile (/=2) (reverse mpHand)))))



----------------- End Two Pair ------------------
---------------- Three of a kind ----------------
hasThreeOfAKind :: [Card] -> (Bool, Maybe Rank, Maybe [Int])
hasThreeOfAKind cards
     | hasThreeOfAKindBool cards = (True, Just (rankThreeOfAKind cards), Just (indexOfThreeOfAKind cards))
     | otherwise            = (False, Nothing, Nothing)


hasThreeOfAKindBool :: [Card] -> Bool
hasThreeOfAKindBool cards
     | trips == 1 = True
     | trips == 2 = True
     | otherwise  = False
     where
          grHand = group (sortRankHandList cards)        
          mpHand = map (length) grHand
          trips  = length (filter (==3) mpHand)


rankThreeOfAKind :: [Card] -> Rank
rankThreeOfAKind cards = rank (cards!!index)
     where
          grHand = group (sortRankHandList cards)        
          mpHand = map (length) grHand
          index = sum (dropWhile (/=3) (reverse mpHand)) -1

indexOfThreeOfAKind :: [Card] -> [Int]
indexOfThreeOfAKind cards = [firstIndex, firstIndex +1, firstIndex+2]
        where
          grHand = group (sortRankHandList cards)        
          mpHand = map (length) grHand
          firstIndex = sum (drop 1 (dropWhile (/=3) (reverse mpHand)))

-------------- End three of a kind --------------








 -- | @output
 -- | Combination -> best combination that can be determined from (hand ++ communitycards)
 -- | Maybe Rank -> if best combination:
                            -- highCard      = Rank of highest card on hand
                            -- pair          = Rank of pair
                            -- twoPairs      = Rank of the highest Rank of the Pairs
                            -- ThreeOfAKind  = Rank of the threeOfAKind
                            -- Straight      = Rank of highest card in the straight 
                            -- Flush         = Nothing 
                            -- FullHouse     = Rank of the highest card on the hand
                            -- Quads         = Rank of the quads
                            -- StraightFlush = Rank of the highest card in the straight
 -- | Rank of Highest card on hand (independent on combination)
 -- | [Card] -> a list of the cards with the best hand

bestCombination :: [Card] -> [Card] -> (Combination, Maybe Rank, Rank, [Card])
bestCombination hand communityCards = undefined

