

module HandEvaluation where

import Cards
import Data.List
import Data.Maybe
import Data.Function (on)


----------------------------
-------- Test Hands --------
----------------------------

hand0 :: Hand
hand0 = [Card Two Hearts, Card Four Spades ]

hand1 :: Hand
hand1 = [ Card Two Hearts, Card Jack Spades]

hand2 :: Hand
hand2 = [ Card Two Hearts, Card Jack Spades, Card Five Clubs]


hand3 :: Hand
hand3 = [ Card Two Spades, Card Five Clubs, Card Two Clubs, Card Jack Spades ]

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
----------------------------
----------------------------

-- | @input Hand
-- | @output list with the cards ranks, sorted
sortRankHandList :: [Card] -> [Rank]
sortRankHandList hand = sort [ rank x | x <- hand]


-- | @input Hand
-- | @output list with the cards suits, sorted
sortSuitHandList :: Hand -> [Suit]
sortSuitHandList hand = sort [ suit x | x <- hand]




tupleList :: [Card] -> [(Suit, Rank)]
tupleList cards = [ (suit x, rank x) | x <- cards]


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
-------------------- Straight -------------------
{-
hasStraight :: [Card] -> (Bool, Maybe Rank, Maybe [Int])
hasStraight cards
     | hasStraightBool cards = (True, Just (rankStraight cards), Just (indexStraight cards))
     | otherwise            = (False, Nothing, Nothing)


hasStraight :: [Card] -> Bool
hasStraight cards
    | length noDuplicates < 5 = False
    |  
    where
        rankHand     = sortRankHandList cards
        noDuplicates = nub rankHand
-}

------------------ End Straight -----------------
---------------------- Flush --------------------

hasFlush :: [Card] -> (Bool, Maybe Rank, Maybe [Int])
hasFlush cards
     | hasFlushBool cards = (True, Nothing, Just (indexOfFlush cards))
     | otherwise          = (False, Nothing, Nothing)

hasFlushBool :: [Card] -> Bool
hasFlushBool cards
          | flush /= 0 = True
          | otherwise   = False
     where
          grHand = group (sortSuitHandList cards)        
          mpHand = map (length) grHand
          flush  = length (filter (>=5) mpHand)


indexOfFlush :: [Card] -> [Int]
indexOfFlush cards = getIndex cards sortedAfterRank
     where
          sortedTupleList   = sort (tupleList cards)
          groupedBySuit     = groupBy ((==) `on` fst) (sortOn fst sortedTupleList)
          flushGroups       = filter ((>=5) . length) groupedBySuit
          highestRankOfSuit = (take 5 (reverse (concat flushGroups)))
          sortedAfterRank   = switch highestRankOfSuit
          


switch :: [(Suit, Rank)] -> [Card]
switch []           = []
switch ((s1,r1):xs) =  (Card r1 s1) : switch xs


getIndex :: [Card] -> [Card] -> [Int]
getIndex xs ys = [ i | (i, x) <- zip [0..] xs, x `elem` ys ]

-------------------- End Flush ------------------
-------------------- Full House -----------------


hasFullHouse :: [Card] -> (Bool, [Maybe Rank], Maybe [Int])
hasFullHouse cards
     | hasFullHouseBool cards = (True,
                                [Just (rankThreeOfAKind cards), Just bestRankPair],
                                 Just ([bestPairIndex cards,
                                       (bestPairIndex cards + 1)] ++
                                       indexOfThreeOfAKind cards)  )

     | otherwise            = (False, [Nothing], Nothing)
     where
          bestRankPair = rank (cards!!(bestPairIndex cards))

-- | Helpers
hasFullHouseBool :: [Card] -> Bool
hasFullHouseBool cards
     | (tripples >= 1) && (pairs >= 1) = True
     | (tripples >=2 ) = True
     | otherwise  = False
     where
          grHand   = group (sortRankHandList cards)        
          mpHand   = map (length) grHand
          pairs    = length (filter (==2) mpHand)
          tripples = length (filter (==3) mpHand) 



bestPairIndex :: [Card] -> Int
bestPairIndex cards = sum (drop 1 (dropWhile (/=2) (reverse mpHand)))
        where
          grHand = group (sortRankHandList cards)        
          mpHand = map (length) grHand
          

------------------ End Full House ---------------
---------------------- Quads --------------------

hasQuads :: [Card] -> (Bool, Maybe Rank, Maybe [Int])
hasQuads cards
     | hasQuadsBool cards = (True, Just (rankQuads cards), Just (indexOfQuads cards))
     | otherwise            = (False, Nothing, Nothing)


hasQuadsBool :: [Card] -> Bool
hasQuadsBool cards
     | quads >= 1 = True
     | otherwise  = False
     where
          grHand = group (sortRankHandList cards)        
          mpHand = map (length) grHand
          quads  = length (filter (==4) mpHand)


rankQuads :: [Card] -> Rank
rankQuads cards = rank (cards!!index)
     where
          grHand = group (sortRankHandList cards)        
          mpHand = map (length) grHand
          index = sum (dropWhile (/=4) (reverse mpHand)) -1

indexOfQuads :: [Card] -> [Int]
indexOfQuads cards = [firstIndex, firstIndex +1, firstIndex+2, firstIndex+3]
        where
          grHand = group (sortRankHandList cards)        
          mpHand = map (length) grHand
          firstIndex = sum (drop 1 (dropWhile (/=4) (reverse mpHand)))

-------------------- End Quads ------------------
----------------- Straight Flush ----------------


hasStraightFlush :: [Card] -> (Bool, Maybe Rank, Maybe [Int])
hasStraightFlush cards = undefined

--------------- End Straight Flush --------------














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

