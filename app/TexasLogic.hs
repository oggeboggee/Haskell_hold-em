
module TexasLogic where

import Data.List

import Cards



{- determination of combination -}

-- TODO
-- not sure if we want to do this "Bool-way", it will be harder to compare two hands with same combination.
-- not sure it we want to sort the hand in the function or outside the function

{-
handsHighCard        :: Hand -> Rank
handHasTwoPair       :: Hand -> Bool
handHasThreeOfAKind  :: Hand -> Bool
handHasStraight      :: Hand -> Bool
handHasFlush         :: Hand -> Bool
handHasFullHouse     :: Hand -> Bool
handHasQuads         :: Hand -> Bool
handHasStraightFlush :: Hand -> Bool
-}


 -- | Hand has to be sorted 
handHasPair :: Hand -> Bool
handHasPair (card1 : card2 : xs) 
     | rank card1 == rank card2    = True 
     | otherwise = handHasPair (card2 : xs)
handHasPair _    = False






