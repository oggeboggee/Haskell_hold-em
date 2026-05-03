module HandEvaluation where

import Cards
import Types
import Data.List
import Data.Maybe
import Data.Function (on)


--------------------
--------------------
{-

functions:
     Flush                        CHECK   
     StraightFlush                 CHECK
     Straight                        CHECK
     fullHouse/quads                    CHECK
     threeOfAKind/twoPairs/pair/highCard   CHECK


check flush
    if flush: check straightFlush                                               CHECK
               if straightFlush --> straightFlush                               CEHCK
               else             --> flush                                       CHECK
    else: check straight                                                        CHECK
               if straight  --> straight                                        CHECK
               else: quads/fullHouse                                            CHECK
                         if quads/fullHouse --> quad/fullHouse                  CHECK
                         else               --> trips/twoPair/pair/highCard     CHECK

-}



------------------------------------------------------------

winnersWithCom :: [Card] -> [[Card]] -> ([Int], Combination)
winnersWithCom com play = (indexes, (fst (handData  (play!!(head  indexes) ++ com))))
     where
          indexes = winners com play

winners :: [Card] -> [[Card]] -> [Int]
winners com play
     | length bestCombIndexes == 1 = bestCombIndexes   -- <-- the case when we have an uniqe winner
     | otherwise                   = compareCards      -- <-- comparision between every card in the hands who shares 'best combination'
                                                       --     [ most valuable <--  --> least valuable ] compares from left to right
     where
         handDataList = map handData $ map (sort) $ map (++ com) play                                   -- [[Card]] -> [ (Combination, [Card]) ]
         bestCombIndexes = findIndices (\x -> fst x == maximum (map fst handDataList)) handDataList                     
         compareCards = handComparision handDataList bestCombIndexes                           

-- | returns the indexes of the best hands, only comparing the hands who matches the indexes in the first list
handComparision :: [(Combination, [Card])] -> [Int] -> [Int]
handComparision list index = bestIds
     where
          remaining         = zip (index) $ map (\(_,h) -> h) (map (\i -> list!!i ) index)    --example: [(c0,h0),(c1,h1),(c2,h2),(c3,h3)] [0,3] -> [ (0,h0),(3,h3)]
          bestHandReference = maximum $ map (\(_, h) -> map rank h) remaining
          bestIds           = [ i | (i, h) <- remaining, map rank h == bestHandReference]
           

handData :: [Card] -> (Combination, [Card])
handData cards = fromJust (flush $ sort cards)
           
---------------------------------------------------------------------
-------------------------new implementions --------------------------

flush :: [Card] -> Maybe (Combination, [Card])
flush cards
     | cards == [] = Nothing
     | length flushes >= 1 = (straightFlush cards (take 5 bestFlush)) 
     | otherwise = straight cards
     where
          sortedBySuit = sortOn suit cards                          -- sorts cards by suit
          groupCards   = groupBy ((==) `on` suit) sortedBySuit      -- groups card by suit
          flushes      = filter (\x -> length x >=5) groupCards     -- removes the groups with length <5
          sortedLists  = map (reverse . sort) flushes               -- sorts every flush-list in dec. order
          bestFlush    = maximumBy (compare `on` head) sortedLists  -- return the best list of flushes

------------------------------------------------------------

straightFlush :: [Card] -> [Card] -> Maybe (Combination, [Card])
straightFlush cards flush
          | maybeIndexHigh   /= Nothing = Just (StraightFlush, take 5 (drop (fromJust maybeIndexHigh) rankSortAceHigh))
          | maybeIndexLow    /= Nothing = Just (StraightFlush, take 5 (drop (fromJust maybeIndexLow)  rankSortAceLow) )
          | otherwise     = Just (Flush, flush)
     where
          sortedBySuit = sortOn suit cards                          -- sorts cards by suit
          groupCards   = groupBy ((==) `on` suit) sortedBySuit      -- groups card by suit
          flushes      = filter (\x -> length x >=5) groupCards     -- removes the groups with length <5

          rankSortAceHigh = reverse $ sortBy (\(Card r1 s1) (Card r2 s2) -> compare (rankValue r1) (rankValue r2)) (concat flushes)
          sfPossibillitiesAceHigh = zip rankSortAceHigh (drop 4 rankSortAceHigh)      
          maybeIndexHigh = findIndex (\(x,y) -> (rankValue $ rank x) - (rankValue $ rank y) == 4) sfPossibillitiesAceHigh  -- Nothing or first Index of straightFlush 

                
          rankSortAceLow = reverse $ sortBy (\(Card r1 s1) (Card r2 s2) -> compare (rankValueAceLow r1) (rankValueAceLow r2)) (concat flushes)
          sfPossibillitiesAceLow = zip rankSortAceLow (drop 4 rankSortAceLow)
          maybeIndexLow = findIndex (\(x,y) -> (rankValueAceLow $ rank x) - (rankValueAceLow $ rank y) == 4) sfPossibillitiesAceLow  -- Nothing or first Index of straightFlush 

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
------------------------------------------------------------


straight :: [Card] -> Maybe (Combination, [Card])
straight cards
     | maybeIndexHigh   /= Nothing = Just (Straight, take 5 (drop (fromJust maybeIndexHigh) rankSortAceHigh))
     | maybeIndexLow    /= Nothing = Just (Straight, take 5 (drop (fromJust maybeIndexLow)  rankSortAceLow) )
     | otherwise = quadsFullhouse cards
     where
          sortedByRank = reverse $ sortOn rank cards 
          noDuplicates = nubBy (\(Card r1 _) (Card r2 _) -> r1 == r2) sortedByRank
          
          rankSortAceHigh = reverse $ sortBy (\(Card r1 _) (Card r2 _) -> compare (rankValue r1) (rankValue r2)) noDuplicates
          straightPossibillitiesAceHigh = zip rankSortAceHigh (drop 4 rankSortAceHigh)      
          maybeIndexHigh = findIndex (\(x,y) -> (rankValue $ rank x) - (rankValue $ rank y) == 4) straightPossibillitiesAceHigh  -- Nothing or first Index of straightFlush 

          rankSortAceLow = reverse $ sortBy (\(Card r1 _) (Card r2 _) -> compare (rankValueAceLow r1) (rankValueAceLow r2)) noDuplicates
          straightPossibillitiesAceLow = zip rankSortAceLow (drop 4 rankSortAceLow)
          maybeIndexLow = findIndex (\(x,y) -> (rankValueAceLow $ rank x) - (rankValueAceLow $ rank y) == 4) straightPossibillitiesAceLow  -- Nothing or first Index of straightFlush 


------------------------------------------------------------

quadsFullhouse :: [Card] -> Maybe (Combination, [Card])
quadsFullhouse cards
     | quadCards /= Nothing = Just (Quads, fromJust (quadCards) ++ highestQuadKick)
     | length fullHouseCards == 5 = Just (FullHouse, fullHouseCards)
     | otherwise = tripsTwoPairHigh cards
     where
        groupCards = groupBy ((==) `on` rank) cards   -- example [[2H,2H], [6C,6C], [8D]]

        quadCards       = find (\g -> length g == 4) groupCards
        highestQuadKick = take 1 (reverse $ sort $ removeCards (fromJust quadCards) cards)

        bestTrips = take 3 $ reverse $ sort $ concat ((filter (\x -> length x == 3) groupCards)) 

        chooseAble      = removeCards bestTrips $ concat (filter (\x -> length x >= 2) groupCards)
        bestPair        = take 2 $ reverse $ sort $ chooseAble
        fullHouseCards   = bestTrips ++ bestPair
          


------------------------------------------------------------

tripsTwoPairHigh :: [Card] -> Maybe (Combination, [Card])
tripsTwoPairHigh cards
     | trip             /= Nothing = Just (ThreeOfAKind, tripCards )
     | length allPairs  >= 2       = Just (TwoPairs, twoPairCards  )
     | length allPairs  == 1       = Just (Pair,     pairCards     )
     | otherwise                   = Just (HighCard, highCardCards )
     where
        groupCards = groupBy ((==) `on` rank) cards

        trip        = find (\g -> length g == 3) groupCards 
        tripKickers = take 2 $ reverse $ sort $ removeCards (fromJust trip) cards
        tripCards   =  (fromJust trip) ++ tripKickers

        allPairs    =  filter (\g -> length g == 2) groupCards
        bestPairs   =  concat $ take 2 $ reverse $ sort allPairs
        twosKicker  =  take 1 $ reverse $ sort $ removeCards bestPairs cards 
        twoPairCards   = bestPairs ++ twosKicker

        bestPair   =  concat $ take 1 $ reverse $ sort allPairs
        pairKickers = take 3 $ reverse $ sort $ removeCards bestPair cards
        pairCards   = bestPair ++ pairKickers

        highCardCards = take 5 $ reverse $ sort cards

        



------------------------------------------------------------


{-
------------------------------------------------------------
---------------------Combinations---------------------------
------------------------------------------------------------
highCard :: [Card] -> Maybe (Combination, [Card])
highCard []    = Nothing
highCard cards = Just (HighCard, cards')
     where
          cards' = take 5 $ reverse $  cards

------------------------------------------------------------
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

------------------------------------------------------------
twoPair :: [Card] -> Maybe (Combination, [Card])
twoPair cards
     | length cards < 4 = Nothing
     | length pairCards >= 2   = Just ( TwoPairs,                               -- <-- Combination
                                      (concat highestTwoPair) ++ kickerCards    -- <-- BestHand with kickers in descending order
                                      )
     | otherwise = Nothing
          where
               groupCards = groupBy ((==) `on` rank) cards            -- example [[2H,2H], [6C,6C], [8D]]
               pairCards = filter (\g -> length g == 2) groupCards    --         [[2H,2H], [6C,6C]]
               highestTwoPair = take 2 $ reverse pairCards
               kickerCards = take 1 $ reverse $ removeCards (concat highestTwoPair) cards

------------------------------------------------------------
threeOfAKind :: [Card] -> Maybe (Combination, [Card])
threeOfAKind cards
     | length cards < 3 = Nothing
     | length tripCards >=1 = Just ( ThreeOfAKind,                    -- <-- Combination
                                    (fromJust tripCards) ++ kickerCards   -- <-- BestHand with kickers in descending order
                                   )
     | otherwise = Nothing
     where
        groupCards = groupBy ((==) `on` rank) cards    
        tripCards  = find (\g -> length g == 3) groupCards 
        kickerCards = take 2 $ reverse $ removeCards (fromJust tripCards) cards

------------------------------------------------------------
straight :: [Card] -> Maybe (Combination, [Card])
straight cards
     | length cards < 5 = Nothing
     | length allStraights' >= 1 = Just ( Straight,                        -- <-- combination
                                          reverse $ last $ allStraights'   -- <-- best straight in descending order
                                          )
     | otherwise = Nothing
     where
       allStraights' = allStraights cards

allStraights :: [Card] -> [[Card]] 
allStraights cards@(x:xs)
     | length cards < 5 = []
     | straightTester (take 5 rankList) == True = (take 5 cards) : allStraights xs
     | otherwise = allStraights xs
     where
          rankList = map rank cards

straightTester :: [Rank] -> Bool
straightTester []  = True
straightTester [_] = True
straightTester (x:y:xs)
     | (rankValue x ==  rankValue y - 1)             = straightTester (y:xs)
     | (rankValueAceLow x == rankValueAceLow y - 1 ) = straightTester (y:xs)
     | otherwise = False



------------------------------------------------------------
flush :: [Card] -> Maybe (Combination, [Card])
flush cards
     | length cards < 5 = Nothing
     | length flushes >= 1 = Just (Flush,                   -- <-- combination
                                      take 5 bestFlush      -- <-- best flush
                                        )
     | otherwise = Nothing 
     where
          sortedBySuit = sortOn suit cards                         -- sorts cards by suit
          groupCards   = groupBy ((==) `on` suit) sortedBySuit     -- groups card by suit
          flushes      = filter (\x -> length x >=5) groupCards    -- removes the groups with length <5
          sortedLists  = map (reverse) (map sort flushes)          -- sorts every flush-list in dec. order
          bestFlush    = maximumBy (compare `on` head) sortedLists -- return the best list of flushes

------------------------------------------------------------
fullHouse :: [Card] -> Maybe (Combination, [Card])
fullHouse cards
          | length cards < 5 = Nothing
          | (length tripList >= 1) && (length (tripList ++ pairList) >= 2) = Just (FullHouse,
                                                                                   bestTrips ++ (take 2 bestPair)
                                                                                   ) 
          | otherwise = Nothing                                                                                   
     where
          groupCards = groupBy ((==) `on` rank) cards                           -- grouped by rank
          tripList   = filter (\x -> length x == 3) groupCards                  -- list with lists of all trips
          pairList   = filter (\x -> length x == 2) groupCards                  -- list with lists of all pairs

          bestTrips           = maximumBy (compare `on` head) tripList                  -- best trip
          selectable         = removeCards bestTrips (concat $ tripList ++ pairList)    -- removes the card choosen for the trip
          selectableInGroups = groupBy ((==) `on` rank) selectable                      -- regroup the cards
          bestPair           = maximumBy (compare `on` head) selectableInGroups        

------------------------------------------------------------
quads :: [Card] -> Maybe (Combination, [Card])
quads cards
     | length cards < 4     = Nothing
     | quadCards /= Nothing = Just ( Quads,                                 -- <-- Combination
                                     (fromJust quadCards) ++ kickerCard     -- <-- BestHand with kickers in descending order
                                    )
     | otherwise = Nothing
     where        
          groupCards = groupBy ((==) `on` rank) cards
          quadCards = find (\g -> length g == 4) groupCards
          kickerCard = take 1 $ reverse $ removeCards (fromJust quadCards) cards

------------------------------------------------------------
straightFlush :: [Card] -> Maybe (Combination, [Card])
straightFlush cards
     | length cards < 5          = Nothing
     | flush cards    == Nothing = Nothing
     | straight cards == Nothing = Nothing
     | straightFlush' /= Nothing = Just (StraightFlush,
                                         take 5 $ reverse $ fromJust straightFlush'
                                         )
     | otherwise                 = Nothing
     where
          sortedBySuit   = sortOn suit cards                         -- sorts cards by suit
          groupCards     = groupBy ((==) `on` suit) sortedBySuit     -- group cards by suit
          flushes = filter (\x -> length x >=5) groupCards    -- removes the groups with length <5
          straightFlush' = find (\x -> straight x /= Nothing) flushes
-}
------------------------------------------------------------
------------------------ helpers ---------------------------

-- |  returns a list of ys without any xs-cards
removeCards :: [Card] -> [Card] -> [Card]
removeCards xs ys = [ y | y <- ys, y `notElem` xs ]

             ----------------------------
             -------- Test Hands --------

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

---------------------------------
----------- HandLists -----------
handList1 :: [[Card]]
handList1 = [hand25, hand26, hand27]

handList2 :: [[Card]]
handList2 = [[Card Five Hearts, Card King Spades], [Card Eight Hearts, Card King Clubs], [Card Two Spades, Card Three Spades]]

----------------------------------
tester :: ([Card] -> Maybe (Combination, [Card])) ->  [([Card], Maybe (Combination, [Card]))]
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
