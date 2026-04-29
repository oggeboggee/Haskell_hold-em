module HandEvaluation where

import Cards
import Types
import Data.List
import Data.Maybe
import Data.Function (on)



----------------------------------------------------------------
--------------------   New Functions  --------------------------
----------------------------------------------------------------

-- | (Combination, [Card], [Card], [Card])
-- | 
-- | 

handData :: [Card] -> (Combination, [Card])
handData cards = undefined



pair :: [Card] -> Maybe (Combination, [Card])
pair cards
     | length cards < 2     = Nothing
     | pairCards /= Nothing = Just ( Pair,                                 -- <-- Combination
                                     (fromJust pairCards) ++ kickerCards   -- <-- BestHand with kickers in descending order
                                    )
     | otherwise = Nothing
     where        
          groupCards = groupBy ((==) `on` rank) cards
          pairCards = find (\g -> length g == 2) groupCards
          kickerCards = take 3 $ reverse $ removeCards (fromJust pairCards) cards

-- Testing:
-- if it not have a pair, it should return Nothing
-- if we have a pair, it should return a pair
          -- the two first cards of the list of cards is the pair
          -- the rest of the cards is three highest cards of the cards who not is part of the pair


twoPair :: [Card] -> Maybe (Combination, [Card])
twoPair cards
     | length cards < 4 = Nothing
     | length pairCards >= 2   = Just ( TwoPairs,                                 -- <-- Combination
                                      (concat highestTwoPair) ++ kickerCards    -- <-- BestHand with kickers in descending order
                                      )
     | otherwise = Nothing
          where
               groupCards = groupBy ((==) `on` rank) cards            -- example [[2H,2H], [6C,6C], [8D]]
               pairCards = filter (\g -> length g == 2) groupCards    --         [[2H,2H], [6C,6C]]
               highestTwoPair = take 2 $ reverse pairCards
               kickerCards = take 1 $ reverse $ removeCards (concat highestTwoPair) cards


-- Testing:
-- if we dont have Two-pair, it should return Nothing
-- if we have two or more pairs, it should return twoPair
          -- the order should always be : [highest pair ++ second highest pair ++ best kicker]




----------------------------------------------------------------
----------------------------------------------------------------
----------------------------------------------------------------




----------------------------
------------helpers---------

 -- | extracts the suits and sorts them
sortSuitHandList :: Hand -> [Suit]
sortSuitHandList hand = sort [ suit x | x <- hand]

 -- | return a list with tuples
tupleList :: [Card] -> [(Suit, Rank)]
tupleList cards = [ (suit x, rank x) | x <- cards]

 -- | returns a list with the indexes of the cards form xs in ys
getIndex :: [Card] -> [Card] -> [Int]
getIndex xs ys = [ i | (i, x) <- zip [0..] xs, x `elem` ys ]

-- | return a sorted list with the cards ranks
sortRankHandList :: [Card] -> [Rank]
sortRankHandList cards = sort [ rank x | x <- cards]


-- | return a sorted list with groups of every rank with number of occurances in every inner list.
-- | example [J♥,J♠,Q♥,K♠,A♥] -> [[J,J],[Q],[K],[A]]
groupAfterRank :: [Card] -> [[Rank]]
groupAfterRank cards = group (sortRankHandList cards)      

-- | returns a list with Int of occurances if every rank
-- | example [J♥,J♠,Q♥,K♠,A♥] -> [2,1,1,1]
sizeByGroup :: [Card] -> [Int]
sizeByGroup cards = map (length) (groupAfterRank cards)

-- |  returns a list of ys without any xs-cards
removeCards :: [Card] -> [Card] -> [Card]
removeCards xs ys = [ y | y <- ys, y `notElem` xs ]


mergeCards :: [Card] -> [Card] -> [Card]
mergeCards cards1 cards2 = cards1 ++ cards2

-- | takes the n last element in a list of cards and return the cards sorted from high to low
-- | example [2♥,4♥,6♥,8♣,10♣,J♦,A♦] 3 -> [A♦,J♦,10♣]
kickers :: [Card] -> Int -> [Card]
kickers cards n = take n (reverse $ sort $ cards)


--------------- End helpers Card Logic -------------
------------------ High Card -----------------


-- | Bool                = if the combination exist in the hand
-- | Maybe Combination   = the combination     
-- | Maybe Rank          = the Rank of the pair or Nothing
-- | Maybe [Rank]        = list of ranks of kickers
-- | [Int]               =  indexes of the Card that belongs to the pair    


highCard :: [Card] -> (Bool, Maybe Combination, Maybe [Rank], Maybe [Rank], Maybe [Card])
highCard []    = (False, Nothing, Nothing, Nothing, Nothing)
highCard cards = (True,                               -- <-- Bool if the hand has HighCard
                  Just (HighCard),                    -- <-- Combination
                  Just [(rankHand!!0)],               -- <-- Best rank of the given combination
                  Just (take 5 rankHand),             -- <-- kickers rank in descending order
                  Just (take 5 $ reverse cards)       -- <-- The 5 card that are used for the combination
                     )
     where
          rankHand = reverse (sortRankHandList cards)


---------------- End High Card ---------------
-------------------- Pair --------------------
-- | works with sorted hand
-- | can give wrong answer if it is called with card that have better combination than pair
{-

pair :: [Card] -> (Bool, Maybe Combination, Maybe [Rank], Maybe [Rank], Maybe [Card])
pair []             = (False, Nothing, Nothing, Nothing, Nothing)
pair cards
     | hasPairBool cards = (True,                                       -- <-- Bool if the hand have the combination
                            Just Pair,                                  -- <-- Combination
                            Just [(rankOfPair cards)],                  -- <-- Best rank of the given combination
                            Just (reverse $ (sortRankHandList kick)),   -- <-- kickers rank in descending order
                            Just ((pairCards cards) ++ kick)            -- <-- The 5 card that are used for the combination
                            )
     | otherwise   = (False, Nothing, Nothing, Nothing, Nothing)
     where
          kick = kickers (removeCards (pairCards cards) cards) 3 
-}

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

{-
twoPair :: [Card] -> (Bool, Maybe Combination, Maybe [Rank], Maybe [Rank], Maybe [Card])
twoPair cards
     | length cards <= 3    = (False, Nothing, Nothing, Nothing, Nothing)
     | hasTwoPairBool cards = (True,                                       -- <-- Bool if the hand have the combination
                              Just TwoPairs,                               -- <-- Combination
                              Just (reverse $ rankOfHighestPairs cards),   -- <-- Best rank of the given combination
                              Just (sortRankHandList kick),                -- <-- kickers rank in descending order
                              Just (kick ++ twoPairsCards cards)           -- <-- The 5 card that are used for the combination
                              )
     | otherwise            = (False, Nothing, Nothing, Nothing, Nothing)
          where
               kick = kickers (removeCards twoPairCardss cards) 1
               twoPairCardss = twoPairsCards cards
-}
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
                                    Just ThreeOfAKind,                        -- <-- Combination
                                    Just [(rankThreeOfAKind cards)],          -- <-- Best rank of the given combination
                                    Just (reverse $ sortRankHandList kick),   -- <-- kickers rank in descending order
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
     | length (allStraights cards) >= 1 = (True,                         -- <-- Bool if the hand have the combination
                                   Just Straight,                        -- <-- Combination
                                   Just [(rank (last(straightCards)))],  -- <-- Best rank of the given combination
                                   Nothing,                              -- <-- kickers rank in descending order
                                   Just (straightCards)                  -- <-- The 5 card that are used for the combination
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
                             Just Flush,                         -- <-- Combination
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
                                Just FullHouse,                                -- <-- Combination
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
                             Just Quads,                       -- <-- Combination
                             Just [(rankQuads cards)],         -- <-- Best rank of the given combination
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
          | hasFlushBool cards == False                  = (False, Nothing, Nothing, Nothing, Nothing)
          | length (allStraights cards) == 0             = (False, Nothing, Nothing, Nothing, Nothing)
          | length (allStraights handWithSameSuit) >= 1  = (True,                                                        -- <-- Bool if the hand have the combination
                                                            Just (StraightFlush),                                        -- <-- Combination
                                                            Just [(rank (last(last (allStraights handWithSameSuit))))],  -- <-- Best rank of the given combination
                                                            Nothing,                                                     -- <-- kickers rank in descending order
                                                            Just (last straightList)                                     -- <-- The 5 card that are used for the combination
                                                           )                                       
          | otherwise                                    = (False, Nothing, Nothing, Nothing, Nothing)
          where
               handWithSameSuit = getCardFromIndex cards (allIndexOfFlush cards)
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
{-
-- | Gives data given a list with cards
handData :: [Card] -> (Bool, Maybe Combination, Maybe [Rank], Maybe [Rank], Maybe [Card])
handData cards
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
-}

----------------- End Best Hand -----------------
----------------- Compare Part ------------------

{-

-- | input: communityCards, list of players Hands
-- | output: index of winning hands
winners :: [Card] -> [[Card]] -> [Int]
winners com play = bestCombinations playersHands
     where
          playersHands = mergeCardList com play -- Combines every players hand with the communityCards
-}
{-
bestCombinations :: [[Card]] -> [Int]
bestCombinations playersHands = finalList
          where
               rankList = map getCombination (map handData playersHands) -- returns a list with the combinations of the hands
               highestComb = maximum rankList  -- highest combination of the hands
               zipped = zip rankList [0..] -- returns ziped with index
               indexList =  map check zipped
               check (x,n)                 -- checks if the combination is the highest, returns the indexes for higest, else -1.
                  | x == highestComb = n
                  | otherwise        = -1
               finalList = filter (/= (-1)) indexList 

-}

-- | example 
-- | cards: [5H,8H,JC,QD,AD]
-- | (x:xs): [[6H,KC],[7C,9D]]
-- | output: [[5H,6H,8H,JC,QD,KC,AD],[5H,7C,8H,9D,JC,QD,AD]]
mergeCardList :: [Card] -> [[Card]] -> [[Card]]
mergeCardList card []           = []
mergeCardList cards (x:xs) = sort (mergeCards cards x)  : mergeCardList cards xs



-------------------------------------------------
-------------------------------------------------
-------------------------------------------------
hand24 :: Hand
hand24 = [Card Five Hearts, Card Eight Hearts, Card Jack Hearts, Card Queen Diamonds, Card Ace Diamonds]

hand25 :: Hand
hand25 = [Card Six Hearts, Card King Hearts]

hand26 :: Hand
hand26 = [Card Seven Hearts, Card Queen Hearts]

hand27 :: Hand
hand27 = [Card Eight Diamonds, Card Queen Clubs]

handList1 :: [[Card]]
handList1 = [hand25, hand26, hand27]


hand28 :: Hand
hand28 = [Card Five Hearts, Card Six Spades, Card Seven Clubs, Card Eight Diamonds, Card Ace Hearts]

handList2 :: [[Card]]
handList2 = [[Card Five Hearts, Card King Spades], [Card Eight Hearts, Card King Clubs], [Card Two Spades, Card Three Spades]]

-------------------- getters ---------------------

getBool :: (Bool, a, b, c, d) -> Bool
getBool (b, _, _, _, _) = b

getCombination :: (a, Maybe Combination , b, c, d) -> Maybe Combination
getCombination (_ , c, _, _, _) = c

getCombinationRank :: (a, b, Maybe [Rank], c, d ) -> Maybe [Rank]
getCombinationRank (_ , _, cr, _, _) = cr

getKickers :: (a, b, c, Maybe [Rank], d ) -> Maybe [Rank]
getKickers (_ , _, _, k, _) = k

getFinalCards :: (a, b, c, d, Maybe [Card]) -> Maybe [Card]
getFinalCards (_ , _, _, _, c) = c


--------------- End Compare Part ----------------


----------------------------
-------- Test Hands --------

hand0 :: Hand
hand0 = [Card Two Hearts, Card Four Spades ]

--hand1 :: Hand
--hand1 = [ Card Two Hearts, Card Jack Spades]

--hand2 :: Hand
--hand2 = [ Card Two Hearts, Card Jack Spades, Card Five Clubs]


--hand3 :: Hand
--hand3 = [ Card Two Spades, Card Five Clubs, Card Two Clubs, Card Jack Spades ]


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

