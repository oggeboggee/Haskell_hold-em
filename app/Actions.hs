{- Logic for the the different actions a player can make -}
module Actions where


import App.TexasLogic
import App.Cards

{-
data Action = 
    Fold
    | Check
    | Bet Int
    | Raise Int
    | Call
 -}  

-- | Change a players fold-status to True
fold :: Player -> Player
fold player = player {folded = True}

-- | Pass the turn to the next player
-- check :: Player -> Player


-- | Increase the pot with a certain amout
-- bet :: Chip -> Pot

-- | Up previous bet 
-- raise :: Chip -> Pot

-- | Match bet to go to next phase
-- call :: Chip -> Pot
