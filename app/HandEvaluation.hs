

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

----------------------------
----------------------------




-- | @input Hand
-- | @output list with the cards suits, sorted
sortSuitHandList :: Hand -> [Suit]
sortSuitHandList hand = sort [ suit x | x <- hand]


tupleList :: [Card] -> [(Suit, Rank)]
tupleList cards = [ (suit x, rank x) | x <- cards]


-- | Bool       = if the combination exist in the hand
-- | Maybe Rank = the Rank of the pair or Nothing
-- | [Int]      =  indexes of the Card that belongs to the pair     



---------------- Extract Cards Logic ----------------


getIndex :: [Card] -> [Card] -> [Int]
getIndex xs ys = [ i | (i, x) <- zip [0..] xs, x `elem` ys ]


-- |  returns a list of ys without any xs-cards
removeCards :: [Card] -> [Card] -> [Card]
removeCards xs ys = [ y | y <- ys, y `notElem` xs ]


mergeCards :: [Card] -> [Card] -> [Card]
mergeCards cards1 cards2 = cards1 ++ cards2


kickers :: [Card] -> Int -> [Card]
kickers cards n = take n (reverse $ sort $ cards)


--------------- End Extract Card Logic -------------
---------------- helpers Card Logic ----------------

-- | return a sorted list with the cards ranks
sortRankHandList :: [Card] -> [Rank]
sortRankHandList cards = sort [ rank x | x <- cards]


-- | return a sorted list with the cards ranks 
groupAfterRank :: [Card] -> [[Rank]]
groupAfterRank cards = group (sortRankHandList cards)      


sizeByGroup :: [Card] -> [Int]
sizeByGroup cards = map (length) (groupAfterRank cards)




--------------- End helpers Card Logic -------------
------------------ High Card -----------------


-- | Bool       = if the combination exist in the hand
-- | Maybe Rank = the Rank of the pair or Nothing
-- | Maybe [Rank] = list of ranks of kickers
-- | [Int]      =  indexes of the Card that belongs to the pair    


highCard :: [Card] -> (Bool, Maybe Rank, Maybe [Rank], Maybe [Card])
highCard []    = (False, Nothing, Nothing, Nothing)
highCard cards = (True,                               -- <-- Bool if the hand has HighCard
                  Just (rankHand!!0),                 -- <-- Best rank of the given combination
                  Just (take 5 rankHand),             -- <-- kickers rank in ascending order
                  Just (take 5 $ reverse cards)       -- <-- The 5 card that are used for the combination
                     )
     where
          rankHand = reverse (sortRankHandList cards)


---------------- End High Card ---------------
-------------------- Pair --------------------
-- | works with sorted hand
-- | can give wrong answer if it is called with card that have better combination than pair


pair :: [Card] -> (Bool, Maybe Rank, Maybe [Rank], Maybe [Card])
pair []             = (False, Nothing, Nothing, Nothing)
pair cards
     | hasPairBool cards = (True,                                       -- <-- Bool if the hand has HighCard
                            Just (rankOfPair cards),                    -- <-- Best rank of the given combination
                            Just (reverse $ (sortRankHandList kick)),   -- <-- kickers rank in ascending order
                            Just ((pairCards cards) ++ kick)            -- <-- The 5 card that are used for the combination
                            )
     | otherwise   = (False, Nothing, Nothing, Nothing)
     where
          kick = kickers (removeCards (pairCards cards) cards) 3 


-- | Helpers
hasPairBool :: [Card] -> Bool
hasPairBool cards
     | pairs >= 1 = True
     | otherwise  = False
     where        
          pairs  = length (filter (==2) (sizeByGroup cards))


rankOfPair :: [Card] -> Rank
rankOfPair cards = rank (cards!!indexOfPair)
         where
          indexOfPair = fromJust (elemIndex 2 (sizeByGroup cards))


pairCards :: [Card] -> [Card]
pairCards cards = [cards!!indexOfPair, cards!!(indexOfPair+1)]
        where
          indexOfPair = fromJust (elemIndex 2 (sizeByGroup ( cards)))



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

hasStraight :: [Card] -> (Bool, Maybe Rank, Maybe [Int])
hasStraight cards
     | length straightList >= 1 = (True,
                                   Just (rank (last(last(straightList)))),
                                   Just (getIndex (last straightList) cards))
     | otherwise            = (False, Nothing, Nothing)
          where
               straightList = allStraights cards


allStraights :: [Card] -> [[Card]] 
allStraights (x:xs)
     | length (x:xs) < 5 = []
     | straightTester (take 5 (sortRankHandList (x:xs))) == True = (take 5 (x:xs)) : allStraights xs
     | otherwise = allStraights xs


straightTester :: [Rank] -> Bool
straightTester []  = True
straightTester [_] = True
straightTester (x:y:xs)
     | (rankValue x ==  rankValue y - 1) = straightTester (y:xs)
     | otherwise = False



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
hasStraightFlush cards
          | hasFlushBool cards == False                     = (False, Nothing, Nothing)
          | length (allStraights cards) == 0                  = (False, Nothing, Nothing)
          | length (allStraights handWithSameSuit) >= 1       = (True, 
                                                               Just (rank (last(last (allStraights handWithSameSuit)))),
                                                               Just (getIndex (last straightList) cards)
                                                               )
          | otherwise                                       = (False, Nothing, Nothing)
          where
               handWithSameSuit = getCardFromIndex cards (allIndexOfFlush cards)
               straightList     = allStraights cards


getCardFromIndex :: [Card] -> [Int] -> [Card]
getCardFromIndex cards indexes =
  [ cards !! i | i <- indexes ]

allIndexOfFlush :: [Card] -> [Int]
allIndexOfFlush cards = getIndex cards (switch (concat flushGroups))
     where
          sortedTupleList   = sort (tupleList cards)
          groupedBySuit     = groupBy ((==) `on` fst) (sortOn fst sortedTupleList)
          flushGroups       = filter ((>=5) . length) groupedBySuit
          

--------------- End Straight Flush --------------
----------------- HandStrength ------------------



 -- | combination for best hand 
 -- | [Rank]      for best Rank/Ranks in the combination
 -- | Rank        for the hands best kicker/HighCard
type Score = (Combination, [Rank], Rank)






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


-- TODO

-- Kickers 
     -- HighCard    CHECK
     -- Pair        CHECK   
     -- TwoPairs       
     -- ThreeOfAKind   
     -- Straight 
     -- Flush 
     -- FullHouse 
     -- Quads 
     -- StraightFlush

-- bestHand, given som card
-- special case for Ace as lowest card in a straight |Ace|Two|Three|Four|Five|

-- QuickCheck - testing, proterties
-- make the functions better and more readable
-- bitwise comparison?
