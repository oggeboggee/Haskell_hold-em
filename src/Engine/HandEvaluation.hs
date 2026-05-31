{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Engine.HandEvaluation where

import Engine.Cards
import Engine.EngineTypes
import Data.List
import Data.Function (on)
import Data.Ord (comparing)


-- Functions heavely inspired on poker-maison
-- https://github.com/therewillbecode/poker-maison/blob/master/server/src/Poker/Game/Hands.hs

------------------------------------------------------------

{- 
flowchart

value
    check Flush

    if Flush:
        check Straight

        if Straight:
            return StraightFlush
        else:
            return Flush

    else:
        check Straight

        if Straight:
            return Straight

        check groups
            (Quads, FullHouse, Trips, TwoPairs, Pair, HighCard)

        return best combination from groups
-}

------------------------------------------------------------

-- | takes a list of cards (Community cards)
--
--  takes a list of lists with cards (the players hands)
--
--  return the index of the player or players that have the best combination according to Texas Hold Em rules
winners :: [Card] -> [[Card]] -> [Int]
winners com play
     | length bestCombIndexes == 1 = bestCombIndexes   --   the case when we have an uniqe winner
     | otherwise                   = compareCards      --   comparision between every card in the hands who shares 'best combination'
                                                       --   [ most valuable <--  --> least valuable ] compares from left to right
     where
         handDataList = map value $ map (sort) $ map (++ com) play                                   -- [[Card]] -> [ (Combination, [Card]) ]
         bestCombIndexes = findIndices (\x -> fst x == maximum (map fst handDataList)) handDataList                     
         compareCards = cardComparision handDataList bestCombIndexes         
------------------------------------------------------------

-- |input [(Combination, [Card])] represent the combination of a players cards, with the cards
--
--  input [int] represent the indexes of the hands we want to compare (the combination of the indexes in this list must have the same combination)
--
--  output: [int] who represent the index or indexes of the best hands of the compared ones
cardComparision :: [(Combination, [Card])] -> [Int] -> [Int]
cardComparision list index = bestIds
     where
          remaining         = zip (index) $ map (\(_,h) -> h) (map (\i -> list!!i ) index)  
          bestHandReference = maximum $ map (\(_, h) -> map rank h) remaining
          bestIds           = [ i | (i, h) <- remaining, map rank h == bestHandReference]



------------------------------------------------------------


-- | takes a list with 7 cards
-- 
--   returns a tuple with best possible combination and the cards corresponding that hand
value :: [Card] -> (Combination, [Card])
value cards = maybe (ifNotFlush cards) ifFlush (maybeFlush cards)


-- | takes a list of 7 cards who not contains the combination Flush
-- 
--   returns a tuple with best possible combination and the cards corresponding that hand
ifNotFlush :: [Card] -> (Combination, [Card])
ifNotFlush cards = maybe (checkGroups cards) (Straight,) (maybeStraight cards)

-- | takes a list of 7 cards who  contains the combination Flush
-- 
--   returns a tuple with best possible combination and the cards corresponding that hand
ifFlush :: [Card] -> (Combination, [Card])
ifFlush cards =
  maybe (Flush, take 5 cards) (StraightFlush,) (maybeStraight cards)
   
------------------------------------------------------------


-- | takes a list of 7 cards
-- 
--   if the list contains a flush: the cards with the suit of the flush is returned
--
--   else: the function returns Nothing
maybeFlush :: [Card] -> Maybe [Card]
maybeFlush cs
  | map rank cs' == [Ace, Five, Four, Three, Two] = Just ((\(x:xs) -> xs ++ [x])cs')
  | length cs' >= 5 = Just cs'
  | otherwise = Nothing
  where
    sortBySuit = sortBy (comparing suit <> flip compare)
    groupBySuit = groupBy ((==) `on` suit)
    cs'@(x:xs) = head $ sortByLength $ groupBySuit $ sortBySuit cs

-- | takes a list with >= 5 cards
-- 
--   if the list contains a Straight: the cards of highest possible straight will be returned
--
--   else: the function returns Nothing
maybeStraight :: [Card] -> Maybe [Card]
maybeStraight cards
  | length cs'' >= 5 = Just (take 5 $ reverse cs'')
  | otherwise = maybeWheel cardsUniqRanks
  where
    cardsUniqRanks = nubBy ((==) `on` rank) $ sort cards
    cs'' = head $ sortByLength $ groupBySuccCards $ sort cardsUniqRanks --sorts by rank


-- | takes a list with >= 5 cards
-- 
--   if the list contains a Wheel ([A,2,3,4,5)]: the cards of the wheel will be returned
--
--   else: the function returns Nothing
maybeWheel :: [Card] -> Maybe [Card]
maybeWheel cards
  | length filteredCards == 5 = Just ((\(x:xs) -> xs ++ [x]) $ reverse filteredCards)
  | otherwise = Nothing
  where
    filteredCards = (flip elem [Ace, Two, Three, Four, Five] . rank) `filter` cards

-- | takes a list with 7 cards
-- 
--   the function returns a tuple with the best possible groupCombination and the corresponding cards
--
--   groupCombinations: Quads, FullHouse, ThreeOfAKind, TwoPairs, Pair and HighCard
checkGroups :: [Card] -> (Combination, [Card])
checkGroups hand = 
    case groupedRankLengths of
      (3:2:_) -> (hRank, take 5 $ concat groups)
      (3:3:_) -> (hRank, take 5 $ concat groups)
      _       -> (hRank, cards)
    where
              groups = sortByLength $ groupBy ((==) `on` rank) $ sort hand
              concatGroups = concat groups
              cards = (take 4 $ concatGroups) ++ take 1 (reverse $ sort $ drop 4 concatGroups)
              groupedRankLengths = length <$> groups
              hRank = evalGroupedRanks groupedRankLengths

-- | representing the number of the same rank
type RankGroup = Int

-- | takes a list of RankGroups, sorted in decending order
--
--   returns best possible combination due to the RankGroups
evalGroupedRanks :: [RankGroup] -> Combination
evalGroupedRanks groups =
     case groups of 
          (4 : _)     -> Quads
          (3 : 2 : _) -> FullHouse
          (3 : 3 : _) -> FullHouse
          (3 : _)     -> ThreeOfAKind
          (2 : 2 : _) -> TwoPairs
          (2 : _)     -> Pair
          _           -> HighCard

-- | takes a list of sorted cards
--
--   Groups cards whose ranks are sequential into the same sublist.
groupBySuccCards :: [Card] -> [[Card]]
groupBySuccCards = foldr f []
  where
    f :: Card -> [[Card]] -> [[Card]]
    f a [] = [[a]]
    f a xs@(x : xs')
      | succ (rank a) == rank (head x) = (a : x) : xs'
      | otherwise = [a] : xs

-- | sorts lists by descending length, breaking ties by lexicographic order.
sortByLength :: Ord a => [[a]] -> [[a]]
sortByLength = sortBy (flip (comparing length) <> flip compare)
