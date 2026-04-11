
module TexasLogic where

import Data.List

import Cards



{- determination of combination -}



 -- | Hand has to be sorted 
handHasPair :: Hand -> Bool
handHasPair (x:y:xs) 
     | x == y    = True 
     | otherwise = handHasPair (y:xs)
handHasPair _    = False

--    working on handHasPair right now, not sure it we want to sort the hand
--    in the function or outside the function


