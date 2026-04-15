module Actions where

{- Logic for the the different actions a player can make -}

import Types
import Cards

{-
data Action = 
    Fold
    | Check
    | Bet Int
    | Raise Int
    | Call
    | AllIn
 -}  

-- | Change a players fold-status to True
fold :: Player -> Player
fold player = player {folded = True}

-- | Pass the turn to the next player
-- check :: Player -> Player

-- | Increase the pot with a certain amout
placeBet :: Player -> Bet -> Table -> (Player, Table)
placeBet player bet pot = 
    (player {chips         = (chips player)-bet,
             commitedChips = (commitedChips player)+bet}, 
     table  {pot = (pot table) + bet})
    

-- | Up previous bet 
-- raise :: Chip -> Pot

-- | Match bet to go to next phase
call :: Player -> Table -> (Player, Table)
call player table = placeBet player lowBet table
    where
        lowBet = lowestBet table player


-- | Helper function to calculate lowest bet a player can make
lowestBet :: Table -> Player -> Bet
lowestBet table player = (highBet table) - (commitedChips player)

-- | A player put all chips in to a pot
allIn :: Player -> Table -> (Player, Table)
allIn player table = placeBet allin table
    where 
        allin = (chips player)

-- allIn player table = 
--     (player {chips = 0, commitedChip = (commitedChips player) + (chips player)},
--      table {pot = (pot table) + (chips player)})






-- | Manual testing:
player1 :: Player
player1 = Player "Axel" hand1 400 0 False NoBlind

table1 :: Table
table1 = Table [player1] 0 fullDeck [] Flop 0