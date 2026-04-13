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
playerBet :: Player -> Int -> Int -> (Player, Int)
playerBet player chip pot = 
    (player {chips = (chips player)-chip}, 
    (pot + chip))
    

-- | Up previous bet 
-- raise :: Chip -> Pot

-- | Match bet to go to next phase
call :: Player -> Bet -> Table -> (Player, Table)
call player bet table = 
    (player {chips = (chips player)-lowBet, 
      commitedChip = (commitedChip player)+lowBet},
     table  {pot = (pot table) + lowBet})
    where
        lowBet = (highBet table) - (commitedChip player)

allIn :: Player -> Table -> (Player, Table)
allIn player table = 
    (player {chips = 0, commitedChip = (commitedChip player) + (chips player)},
     table {pot = (pot table) + (chips player)})






-- | Manual testing:
player1 :: Player
player1 = Player "Axel" hand1 400 0 False NoBlind

table1 :: Table
table1 = Table [player1] 0 fullDeck [] Flop 0