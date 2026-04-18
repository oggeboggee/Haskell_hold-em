

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

hand22 :: Hand -- Pair
hand22 = [Card Two Hearts, Card Four Hearts, Card Six Hearts, Card Six Clubs, Card Seven Hearts, Card Jack Hearts, Card Queen Hearts]






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


highCard :: [Card] -> (Bool, Maybe Combination, Maybe [Rank], Maybe [Rank], Maybe [Card])
highCard []    = (False, Nothing, Nothing, Nothing, Nothing)
highCard cards = (True,                               -- <-- Bool if the hand has HighCard
                  Just (HighCard),
                  Just [(rankHand!!0)],                 -- <-- Best rank of the given combination
                  Just (take 5 rankHand),             -- <-- kickers rank in descending order
                  Just (take 5 $ reverse cards)       -- <-- The 5 card that are used for the combination
                     )
     where
          rankHand = reverse (sortRankHandList cards)


---------------- End High Card ---------------
-------------------- Pair --------------------
-- | works with sorted hand
-- | can give wrong answer if it is called with card that have better combination than pair


pair :: [Card] -> (Bool, Maybe Combination, Maybe [Rank], Maybe [Rank], Maybe [Card])
pair []             = (False, Nothing, Nothing, Nothing, Nothing)
pair cards
     | hasPairBool cards = (True,                                       -- <-- Bool if the hand have the combination
                            Just Pair,
                            Just [(rankOfPair cards)],                    -- <-- Best rank of the given combination
                            Just (reverse $ (sortRankHandList kick)),   -- <-- kickers rank in descending order
                            Just ((pairCards cards) ++ kick)            -- <-- The 5 card that are used for the combination
                            )
     | otherwise   = (False, Nothing, Nothing, Nothing, Nothing)
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


twoPair :: [Card] -> (Bool, Maybe Combination, Maybe [Rank], Maybe [Rank], Maybe [Card])
twoPair cards
     | length cards <= 3    = (False, Nothing, Nothing, Nothing, Nothing)
     | hasTwoPairBool cards = (True,                                       -- <-- Bool if the hand have the combination
                              Just TwoPairs,
                              Just (reverse $ rankOfHighestPairs cards),   -- <-- Best rank of the given combination
                              Just (sortRankHandList kick),                -- <-- kickers rank in descending order
                              Just (kick ++ twoPairsCards cards)           -- <-- The 5 card that are used for the combination
                              )
     | otherwise            = (False, Nothing, Nothing, Nothing, Nothing)
          where
               kick = kickers (removeCards twoPairCardss cards) 1
               twoPairCardss = twoPairsCards cards

-- | Helpers
hasTwoPairBool :: [Card] -> Bool
hasTwoPairBool cards
     | pairs >= 2 = True
     | otherwise  = False
     where
          pairs  = length (filter (==2) (sizeByGroup cards))


rankOfHighestPairs :: [Card] -> [Rank]
rankOfHighestPairs cards = [rank ((twoPairsCards cards)!!0),
                             rank (last $ twoPairsCards cards)]



twoPairsCards :: [Card] -> [Card]
twoPairsCards cards = [cards!!indexFPair,
                       cards!!(indexFPair+1),  
                       cards!!indexSPair,
                       cards!!(indexSPair+1)
                       ]
             where
          indexSPair = sum (drop 1 (dropWhile (/=2) (reverse (sizeByGroup cards))))
          indexFPair = sum (drop 1 (dropWhile (/=2) (drop 1 (dropWhile (/=2) (reverse (sizeByGroup cards))))))


----------------- End Two Pair ------------------
---------------- Three of a kind ----------------

threeOfAKind :: [Card] -> (Bool, Maybe Combination, Maybe [Rank], Maybe [Rank], Maybe [Card])
threeOfAKind cards
     | length cards < 3 = (False, Nothing, Nothing, Nothing, Nothing)
     | hasThreeOfAKindBool cards = (True,                                     -- <-- Bool if the hand have the combination
                                    Just ThreeOfAKind,
                                    Just [(rankThreeOfAKind cards)],            -- <-- Best rank of the given combination
                                    Just (reverse $ sortRankHandList kick),             -- <-- kickers rank in descending order
                                    Just (kick ++ (cardsThreeOfAKind cards))  -- <-- The 5 card that are used for the combination
                                    )
     | otherwise            = (False, Nothing, Nothing, Nothing, Nothing)       
          where
            kick = kickers (removeCards triCards cards) 2
            triCards = cardsThreeOfAKind cards



hasThreeOfAKindBool :: [Card] -> Bool
hasThreeOfAKindBool cards
     | trips >= 1 = True
     | otherwise  = False
     where
          trips  = length (filter (==3) (sizeByGroup cards))


rankThreeOfAKind :: [Card] -> Rank
rankThreeOfAKind cards = rank (cards!!index)
     where
          index = sum (dropWhile (/=3) (reverse (sizeByGroup cards))) -1

cardsThreeOfAKind :: [Card] -> [Card]
cardsThreeOfAKind cards = [ cards!!firstIndex,
                            cards!!(firstIndex+1),
                            cards!!(firstIndex+2)
                         ]
          where
            firstIndex = sum (drop 1 (dropWhile (/=3) (reverse (sizeByGroup cards))))               

-------------- End three of a kind --------------
-------------------- Straight -------------------

straight :: [Card] -> (Bool, Maybe Combination, Maybe [Rank], Maybe [Rank], Maybe [Card])
straight cards
     | length cards < 5 = (False, Nothing, Nothing, Nothing, Nothing)
     | length (allStraights cards) >= 1 = (True,                       -- <-- Bool if the hand have the combination
                                   Just Straight,
                                   Just [(rank (last(straightCards)))],  -- <-- Best rank of the given combination
                                   Nothing,                            -- <-- kickers rank in descending order
                                   Just (straightCards)                -- <-- The 5 card that are used for the combination
                                   )
     | otherwise            = (False, Nothing, Nothing, Nothing, Nothing)
          where
               straightCards = last (allStraights cards)


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


rankValueAceLow :: Rank -> Int
rankValueAceLow r = case r of
  Ace   -> 1
  Two   -> 2
  Three -> 3
  Four  -> 4
  Five  -> 5
  Six   -> 6
  Seven -> 7
  Eight -> 8
  Nine  -> 9
  Ten   -> 10
  Jack  -> 11
  Queen -> 12
  King  -> 13
  

------------------ End Straight -----------------
---------------------- Flush --------------------

flush :: [Card] -> (Bool, Maybe Combination, Maybe [Rank], Maybe [Rank], Maybe [Card])
flush cards
     | length cards < 5 = (False, Nothing, Nothing, Nothing, Nothing)
     | hasFlushBool cards = (True,                               -- <-- Bool if the hand have the combination
                             Just Flush,
                             Nothing,                            -- <-- Best rank of the given combination
                             Nothing,                            -- <-- kickers rank in descending order
                             Just flushCards )                   -- <-- The 5 card that are used for the combination
     | otherwise          = (False, Nothing, Nothing, Nothing, Nothing)      
          where
               indexes = indexOfFlush cards
               flushCards = [cards!!i | i <- indexes]      

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


fullHouse :: [Card] -> (Bool, Maybe Combination, Maybe [Rank], Maybe [Rank], Maybe [Card])
fullHouse cards
     | hasFullHouseBool cards = (True,                                         -- <-- Bool if the hand have the combination
                                Just FullHouse,
                                Just [(rankThreeOfAKind cards), bestRankPair], -- <-- Best rank of the given combination
                                 Nothing,                                      -- <-- kickers rank in descending order
                                 Just ([cards!!bestPairIndex',                 -- <-- The 5 card that are used for the combination
                                        cards!!(bestPairIndex' + 1)] ++ 
                                        cardsThreeOfAKind cards)) 

     | otherwise            = (False, Nothing, Nothing, Nothing, Nothing)
     where
          bestPairIndex' = bestPairIndex cards
          bestRankPair = rank (cards!!(bestPairIndex'))
          --triCards = [cards!!i | i <- (indexOfThreeOfAKind cards)] 

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

quads :: [Card] -> (Bool, Maybe Combination, Maybe [Rank], Maybe [Rank], Maybe [Card])
quads cards
     | hasQuadsBool cards = (True,                             -- <-- Bool if the hand have the combination
                             Just Quads,
                             Just [(rankQuads cards)],           -- <-- Best rank of the given combination
                             Just (sortRankHandList kick),     -- <-- kickers rank in descending order
                             Just (kick ++ cardOfQuads cards)  -- <-- The 5 card that are used for the combination
                             )                              
     | otherwise            = (False, Nothing, Nothing, Nothing, Nothing)
          where
               kick = kickers (removeCards (cardOfQuads cards) (cards)) 1

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
          index = sum (dropWhile (/=4) (reverse (sizeByGroup cards))) -1

cardOfQuads :: [Card] -> [Card]
cardOfQuads cards = [ cards!!(firstIndex), 
                      cards!!(firstIndex +1), 
                      cards!!(firstIndex+2), 
                      cards!!(firstIndex+3)
                      ]
        where
          firstIndex = sum (drop 1 (dropWhile (/=4) (reverse $ sizeByGroup cards)))

-------------------- End Quads ------------------
----------------- Straight Flush ----------------


straightFlush :: [Card] -> (Bool, Maybe Combination, Maybe [Rank], Maybe [Rank], Maybe [Card])
straightFlush cards
          | hasFlushBool cards == False                       = (False, Nothing, Nothing, Nothing, Nothing)
          | length (allStraights cards) == 0                  = (False, Nothing, Nothing, Nothing, Nothing)
          | length (allStraights handWithSameSuit) >= 1       = (True,                                                          -- <-- Bool if the hand have the combination
                                                               Just (StraightFlush),
                                                               Just [(rank (last(last (allStraights handWithSameSuit))))],        -- <-- Best rank of the given combination
                                                               Nothing,                                                         -- <-- kickers rank in descending order
                                                               Just (last straightList) -- <-- The 5 card that are used for the combination
                                                                )                                       
          | otherwise                                       = (False, Nothing, Nothing, Nothing, Nothing)
          where
               handWithSameSuit = getCardFromIndex cards (allIndexOfFlush cards)   -- Creates a  
               straightList     = allStraights cards


getCardFromIndex :: [Card] -> [Int] -> [Card]
getCardFromIndex cards indexes =
  [ cards !! i | i <- indexes ]
  

 -- | returns a list with indexes over cards that have the same suit if there is a suit with 5 or more occurances
allIndexOfFlush :: [Card] -> [Int]
allIndexOfFlush cards = getIndex cards (switch (concat flushGroups))
     where
          sortedTupleList   = sort (tupleList cards)
          groupedBySuit     = groupBy ((==) `on` fst) (sortOn fst sortedTupleList)
          flushGroups       = filter ((>=5) . length) groupedBySuit
          

--------------- End Straight Flush --------------
------------------- Best Hand -------------------


bestHand :: [Card] -> (Bool, Maybe Combination, Maybe [Rank], Maybe [Rank], Maybe [Card])
bestHand cards
     | getBool (straightFlush cards) = straightFlush cards
     | getBool (quads cards        ) = quads cards
     | getBool (fullHouse cards    ) = fullHouse cards
     | getBool (straight cards     ) = straight cards     
     | getBool (flush cards        ) = flush cards
     | getBool (straight cards     ) = straight cards
     | getBool (threeOfAKind cards ) = threeOfAKind cards
     | getBool (twoPair cards      ) = twoPair cards
     | getBool (pair cards         ) = pair cards
     | otherwise                    = highCard cards  


 -- | call with for example (flush hand19) 
 -- | for extracting of the bool-returnvalue 
getBool :: (Bool, a, b, c, d) -> Bool
getBool (b, _, _, _, _) = b


----------------- End Best Hand -----------------



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


-- bestHand, given some card
-- special case for Ace as lowest card in a straight |Ace|Two|Three|Four|Five|

-- QuickCheck - testing, proterties
-- make the functions better and more readable
-- bitwise comparison?
