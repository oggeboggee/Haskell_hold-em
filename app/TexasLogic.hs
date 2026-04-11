
module TexasLogic where

import Data.List

import Cards



{- determination of combination -}

-- TODO
-- not sure if we want to do this "Bool-way", it will be harder to compare two hands with same combination.
-- not sure it we want to sort the hand in the function or outside the function

{-

handHasStraight      :: Hand -> Bool

-}


-- | @input Hand
-- | @output list with the cards ranks, sorted
sortRankHandList :: Hand -> [Rank]
sortRankHandList hand = sort [ rank x | x <- hand]

-- | @input Hand
-- | @output list with the cards suits, sorted
sortSuitHandList :: Hand -> [Suit]
sortSuitHandList hand = sort [ suit x | x <- hand]

 -- | Hand has to be sorted
 -- | Returns Highest rank on hand
handsHighCard :: Hand -> Rank
handsHighCard hand = rank (last hand)

 -- | Hand has to be sorted 
handHasPair :: Hand -> Bool
handHasPair (card1 : card2 : xs) 
     | rank card1 == rank card2    = True 
     | otherwise                   = handHasPair (card2 : xs)
handHasPair _                       = False


handHasTwoPairs :: Hand -> Bool
handHasTwoPairs hand
     | pairs == 2 = True
     | otherwise  = False
     where
          grHand = group (sortRankHandList hand)        
          mpHand = map (length) grHand
          pairs  = length (filter (==2) mpHand)


 -- | Hand has to be sorted
handHasThreeOfAKind  :: Hand -> Bool
handHasThreeOfAKind (card1: card2: card3: xs)
     | (rank card1 == rank card2) && (rank card1 == rank card3) = True
     | otherwise = handHasThreeOfAKind (card2: card3: xs)
handHasThreeOfAKind _ = False


 -- | Hand has to be sorted
handHasQuads :: Hand -> Bool
handHasQuads hand
          | length (grHand!!0) == 4 || length (grHand!!1) == 4 = True
          | otherwise = False
     where
          grHand = group (sortRankHandList hand)


handHasFullHouse :: Hand -> Bool
handHasFullHouse hand
          | length (grHand!!0) == 2 && length (grHand!!1) == 3 || length (grHand!!0) == 3 && length (grHand!!1) == 2 = True
          | otherwise = False
     where
          grHand = group (sortRankHandList hand)



handHasFlush :: Hand -> Bool
handHasFlush hand 
     | length (grHand!!0) == 5 = True
     | otherwise = False
     where
          grHand = group (sortSuitHandList hand)


-- handHasStraightFlush :: Hand -> Bool
-- handHasStraightFlush hand = (handHasFlush hand) && (handHasStraight hand)







