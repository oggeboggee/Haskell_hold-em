module Actions where

{- Logic for the the different actions a player can make -}

import Types
import Cards

import Control.Monad.State


-- | Change a players fold-status to True
fold :: Player -> State Table ()
fold player = do
    table <- get
    let players' = [if (name p) == (name player) 
                    then p {folded = True} 
                    else p| p <- (players table)]
    put table {players = players'}

-- | Pass the turn to the next player
-- check :: Player -> Player
    

--------------------------------------------------------------
-- | Pure helperfunction to decrease a players chips and increase commitedChips
    -- Split up even more:
playerBet :: Player -> Bet -> Player
playerBet player bet = 
    player {chips         = (chips player)-bet,
            commitedChips = (commitedChips player)+bet}

-- 
--------------------------------------------------------------
-- | find and update player chips and commitedChip
    -- Use map...
updateBetPlayers :: String -> Bet -> [Player] -> [Player]
updateBetPlayers player bet players = 
    [if (name p) == player then playerBet p bet else p| p <- (players)]

-- | State monad function to place a bet
placeBet :: Player -> Bet -> State Table ()
placeBet player bet = do
    table    <- get
    let players' = updateBetPlayers (name player) bet (players table)
        pot'     = (pot table) + bet
    put table {players = players', pot = pot'}
--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
-- | New variation with more State monad use
decPlayerChips :: String -> Bet -> State Table ()
decPlayerChips player bet = do
    table <- get
    put table {players = [if (name p) == player
                            then playerBet p bet  
                            else p | p <- (players table)]}

incPotM :: Chip -> State Table ()
incPotM bet = do
    table <- get
    let pot' = (pot table) + bet
    put table {pot = pot'}

placeBet' :: Player -> Bet -> State Table ()
placeBet' player bet = do
    decPlayerChips (name player) bet
    incPotM bet
--------------------------------------------------------------
--------------------------------------------------------------

-- | Take an int for how much to raise, then adds the lowest bet
raise :: Player -> Bet -> State Table ()
raise player raiseamout = do
    table <- get
    let bet = raiseamout + lowestBet table player
    placeBet player bet

--------------------------------------------------------------
-- | A player bet the lowest amount they can to get to next phase
call :: Player -> State Table ()
call player = do
    table <- get
    let lowBet = lowestBet table player
    placeBet player lowBet
    

-- | Helper function to calculate lowest bet a player can make
lowestBet :: Table -> Player -> Bet
lowestBet table player = (highBet table) - (commitedChips player)

--------------------------------------------------------------
-- | Bet all chips
allIn :: Player -> State Table ()
allIn player = do
    placeBet player (chips player)


--------------------------------------------------------------
--------------------------------------------------------------
-- | Manual testing:
player1 :: Player
player1 = Player "Axel" hand1 400 0 False NoBlind

player2 :: Player
player2 = Player "Frodo" hand2 340 0 False NoBlind

playerlist = [player1, player2]

table1 :: Table
table1 = Table playerlist (name (playerlist!!0), 0) 100 fullDeck [] Flop 100


--------------------------------------------------------------
--------------------------------------------------------------
--- Some pure utility functions

-- nextPlayer :: State Table ()
-- nextPlayer = do
    
-- | Decrease the amount of chips a player have by a certain amount
decChip :: Player -> Bet -> Player
decChip player bet = player {chips = (chips player)-bet}

-- | increase the amount of chips a player have by a certain amount
incChip :: Player -> Pot -> Player
incChip player pot = player {chips = (chips player)+pot}

-- | increase the chips a player have commited by a certain amount
incCommitedChip :: Player -> Bet -> Player
incCommitedChip player bet = player {commitedChips = (commitedChips player)+bet}

-- | Reset the commited chips a player have made to zero
resetCommitedChips :: Player -> Player
resetCommitedChips player = player {commitedChips = 0}

-- | Increase the Pot by a certain amount
incPot :: Pot -> Bet -> Pot
incPot pot bet = pot + bet









-- | State Table Table -> (Table, Table)