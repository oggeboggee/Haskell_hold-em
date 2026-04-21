module Actions where

{- Logic for the the different actions a player can make -}

import Types
import Cards ( fullDeck, hand1, hand2, hand3 )

import Control.Monad.State


 {-   
--------------------------------------------------------------
--------------------------------------------------------------
-- | Change whos turn it is
nextPlayerTurn :: State Table ()
nextPlayerTurn = do
    table <- get
    let nextplayer = nextPlayer (snd (playerTurn table)) (activePlayers table)
    put table {playerTurn = nextplayer}
-}
--------------------------------------------------------------
--------------------------------------------------------------
-- | State monad function to place a bet
-- placeBet :: Player -> Bet -> State Table ()
-- placeBet player bet = do
--     table <- get
--     let players' = [if (name p) == (name player )
--                     then decChips p bet 
--                     else p | p <- (players table)]
--         pot'     = (pot table) + bet
--     put table {players = players', pot = pot'}
{-
--------------------------------------------------------------
--------------------------------------------------------------
-- | New variation with more State monad use
-- Helper function that decrease a players chips and change the state
decChips' :: Player -> Bet -> State Table ()
decChips' player bet = do
    table <- get
    put table {players = [if (name p) == (name player)
                          then decChips p bet  
                          else p | p <- (players table)]}

-- Helper function that increase a tables pot and change the state
incPot' :: Chip -> State Table ()
incPot' bet = do
    table <- get
    put table {pot = (pot table) + bet}

-- Transfer a bet from a player to the table pot
placeBet :: Player -> Bet -> State Table ()
placeBet player bet = do
    decChips' player bet
    incPot' bet
    -}

--- Thought: In placeBet we are doing get and put on the table twice, is this inefficient?
--- 

-- placeBet that works with Gaem() 
placeBet :: Int -> Bet -> Game()
placeBet playerIndex bet = do
    modify (\table ->
        let playersAtTable = (players table)
            currentPlayer = playersAtTable !! playerIndex
            updatedPlayer = currentPlayer
                { chips = (chips currentPlayer) - bet,
                  commitedChips = (commitedChips currentPlayer) + bet
                }
            playersAtTableUpdated = replacePlayer playerIndex updatedPlayer playersAtTable

        in table
            { players = playersAtTableUpdated,
              pot = (pot table) + bet,
              bets = bet : (bets table)
            }
        )


-- | We need a function to take a player at a specific index in a list, and then replace with the updated player
--   
replacePlayer :: Int -> Player -> [Player] -> [Player]
replacePlayer playerIndex updatedPlayer playerList = take playerIndex playerList ++ [updatedPlayer] ++ drop (playerIndex + 1) playerList
{-
--------------------------------------------------------------
-------------- All Actions a player can make -----------------
-- All actions end by passing the turn

-- | Change a players fold-status to True
fold :: Player -> State Table ()
fold player = do
    table <- get
    let players' = [if (name p) == (name player) 
                    then p {folded = True} 
                    else p | p <- (players table)]
        active' = filter (not . folded) players'
    put table 
        { players = players',
          activePlayers = active'}
    --nextPlayerTurn



---------------------------------------    
-- | Pass the turn to the next player
--check :: Player -> State Table ()
--check player = do 
    --nextPlayerTurn
---------------------------------------    

-- | Take an int for how much to raise, then adds the lowest bet
raise :: Player -> Bet -> State Table ()
raise player raiseamout = do
    table <- get
    let bet = raiseamout + lowestBet table player
    placeBet player bet
    --nextPlayerTurn

---------------------------------------
-- | A player bet the lowest amount they can to get to next phase
call :: Player -> State Table ()
call player = do
    table <- get
    let lowBet = lowestBet table player
    placeBet player lowBet
    --nextPlayerTurn

---------------------------------------
-- | Bet all chips
allIn :: Player -> State Table ()
allIn player = do
    placeBet player (chips player)
    --nextPlayerTurn
--------------------------------------------------------------
--------------------------------------------------------------

--------------------------------------------------------------
--------------------------------------------------------------
-- Some pure utility functions
-- Dont consider negative numbers
nextPlayer :: Int -> [Player] -> (Player, Int)
nextPlayer x players 
                | i < length players = (players!!i, i)
                | otherwise = (players!!0, 0)
                where
                    i = x+1
    
-- | Pure function, decrease the amount of chips a player have by a certain amount
decChips :: Player -> Bet -> Player
decChips player bet = player {chips         = (chips player)-bet,
                              commitedChips = (commitedChips player)+bet}

-- | increase the amount of chips a player have by a certain amount
incChip :: Player -> Pot -> Player
incChip player pot = player {chips = (chips player)+pot}

-- | Reset the commited chips a player have made to zero
resetCommited :: Player -> Player
resetCommited player = player {commitedChips = 0}

-- | Increase the Pot by a certain amount
incPot :: Pot -> Bet -> Pot
incPot pot bet = pot + bet

-- | Helper function to calculate lowest bet a player can make
lowestBet :: Table -> Player -> Bet
lowestBet table player = (highBet table) - (commitedChips player)




--- We need to apply an action to the game

{-
--------------------------------------------------------------
--------------------------------------------------------------
-- | Manual testing:
player1 :: Player
player1 = Player "Axel" hand1 400 0 False NoBlind

player2 :: Player
player2 = Player "Frodo" hand2 340 0 False NoBlind

player3 :: Player
player3 = Player "Sam" hand3 530 0 False NoBlind

playerlist :: [Player]
playerlist = [player1, player2, player3]

table1 :: Table
table1 = Table playerlist (playerlist!!0, 0) playerlist 50 fullDeck [] Flop 75
-}

-}