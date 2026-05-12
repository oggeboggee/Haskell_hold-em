{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module HandEvaluation where

import Cards
import Types
import Data.List
import Data.Maybe
import Data.Function (on)
import Data.Ord (comparing)


------------------------------------------------------------

{- flowchart 

value (checks if Flush)
    if Flush: 
        check Straight
              if Straight -> return straightFlush
              else        -> return Flush
    
    else:
        check Straight
              if Straight -> return Straight
        check groups (quads, fullHouse, trips, TwoPairs, Pair, HighCard)
              return best combinaion from group-combinations

-}

------------------------------------------------------------
winners :: [Card] -> [[Card]] -> [Int]
winners com play
     | length bestCombIndexes == 1 = bestCombIndexes   -- <-- the case when we have an uniqe winner
     | otherwise                   = compareCards      -- <-- comparision between every card in the hands who shares 'best combination'
                                                       --     [ most valuable <--  --> least valuable ] compares from left to right
     where
         handDataList = map value $ map (sort) $ map (++ com) play                                   -- [[Card]] -> [ (Combination, [Card]) ]
         bestCombIndexes = findIndices (\x -> fst x == maximum (map fst handDataList)) handDataList                     
         compareCards = handComparision handDataList bestCombIndexes         
------------------------------------------------------------


handComparision :: [(Combination, [Card])] -> [Int] -> [Int]
handComparision list index = bestIds
     where
          remaining         = zip (index) $ map (\(_,h) -> h) (map (\i -> list!!i ) index)  
          bestHandReference = maximum $ map (\(_, h) -> map rank h) remaining
          bestIds           = [ i | (i, h) <- remaining, map rank h == bestHandReference]


------------------------------------------------------------

type RankGroup = Int

value :: [Card] -> (Combination, [Card])
value cards = maybe (ifNotFlush cards) ifFlush (maybeFlush cards)

ifNotFlush :: [Card] -> (Combination, [Card])
ifNotFlush cards = maybe (checkGroups cards) (Straight,) (maybeStraight cards)

ifFlush :: [Card] -> (Combination, [Card])
ifFlush cards =
  maybe (Flush, take 5 cards) (StraightFlush,) (maybeStraight cards)

------------------------------------------------------------

lastNelems :: Int -> [a] -> [a]
lastNelems n xs = foldl' (const . drop 1) xs (drop n xs)





maybeFlush :: [Card] -> Maybe [Card]
maybeFlush cs
  | length cs' >= 5 = Just cs'
  | otherwise = Nothing
  where
    sortBySuit = sortBy (comparing suit <> flip compare)
    groupBySuit = groupBy ((==) `on` suit)
    cs' = head $ sortByLength $ groupBySuit $ sortBySuit cs

maybeStraight :: [Card] -> Maybe [Card]
maybeStraight cards
  | length cs'' >= 5 = Just (reverse $ lastNelems 5 cs'')
  | otherwise = maybeWheel cardsUniqRanks
  where
    cardsUniqRanks = nubBy ((==) `on` rank) cards
    cs'' = head $ sortByLength $ groupBySuccCards $ sort cardsUniqRanks

maybeWheel :: [Card] -> Maybe [Card]
maybeWheel cards
  | length filteredCards == 5 = Just filteredCards
  | otherwise = Nothing
  where
    filteredCards =
      (flip elem [Ace, Two, Three, Four, Five] . rank) `filter` cards

checkGroups :: [Card] -> (Combination, [Card])
checkGroups hand = (hRank, cards)
  where
    groups = sortByLength $ groupBy ((==) `on` rank) $ sort hand
    cards = take 5 $ concat groups
    groupedRankLengths = length <$> groups
    hRank = evalGroupedRanks groupedRankLengths

evalGroupedRanks :: [RankGroup] -> Combination
evalGroupedRanks groups =
     case groups of 
          (4 : _)     -> Quads
          (3 : 2 : _) -> FullHouse
          (3 : _)     -> ThreeOfAKind
          (2 : 2 : _) -> TwoPairs
          (2 : _)     -> Pair
          _           -> HighCard


groupBySuccCards :: [Card] -> [[Card]]
groupBySuccCards = foldr f []
  where
    f :: Card -> [[Card]] -> [[Card]]
    f a [] = [[a]]
    f a xs@(x : xs')
      | succ (rank a) == rank (head x) = (a : x) : xs'
      | otherwise = [a] : xs

sortByLength :: Ord a => [[a]] -> [[a]]
sortByLength = sortBy (flip (comparing length) <> flip compare)
