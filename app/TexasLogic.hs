
module TexasLogic where

import Data.List

import Cards



{- determination of combination -}

-- TODO
-- not sure if we want to do this "Bool-way", it will be harder to compare two hands with same combination.
-- not sure it we want to sort the hand in the function or outside the function

{-

handHasTwoPair       :: Hand -> Bool
handHasStraight      :: Hand -> Bool
handHasFlush         :: Hand -> Bool
handHasFullHouse     :: Hand -> Bool
handHasQuads         :: Hand -> Bool
handHasStraightFlush :: Hand -> Bool
-}

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



 -- | Hand has to be sorted
handHasThreeOfAKind  :: Hand -> Bool
handHasThreeOfAKind hand@(card1: card2: card3: xs)
     | (rank card1 == rank card2) && (rank card1 == rank card3) = True
     | otherwise = handHasThreeOfAKind (card2: card3: xs)
handHasThreeOfAKind _ = False







