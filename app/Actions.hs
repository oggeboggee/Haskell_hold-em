module Actions where

{- Logic for the the different actions a player can make -}

import Types
import Cards ( fullDeck, hand1, hand2, hand3 )

import Control.Monad.State
import Data.Bits (Bits(xor))


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

--------------------------------------------------------------
--------------------------------------------------------------
-- | New variation with more State monad use
-- Helper function that decrease a players chips and change the state
-- decChips' :: Player -> Bet -> State Table ()
-- decChips' player bet = do
--     table <- get
--     put table {players = [if (name p) == (name player)
--                           then decChips p bet  
--                           else p | p <- (players table)]}

-- -- Helper function that increase a tables pot and change the state
-- incPot' :: Chip -> State Table ()
-- incPot' bet = do
--     table <- get
--     put table {pot = (pot table) + bet}

-- saveBet :: Bet -> State Table ()
-- saveBet bet = do
--     modify (\table -> table {bets = bet : bets table})

-- -- Transfer a bet from a player to the table pot
-- placeBet' :: Player -> Bet -> State Table ()
-- placeBet' player bet = do
--     decChips' player bet
--     incPot' bet
--     saveBet bet
    

--- Thought: In placeBet we are doing get and put on the table twice, is this inefficient?
--- 

-- placeBet that works with Gaem() 
placeBet :: Int -> Bet -> State Table ()
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
              bets = bet : (bets table),
              highBet = max (highBet table) bet
            }
        )

-- | We need a function to take a player at a specific index in a list, 
--   and then replace with the updated player
replacePlayer :: Int -> Player -> [Player] -> [Player]
replacePlayer playerIndex updatedPlayer playerList = 
    take playerIndex playerList ++ [updatedPlayer] ++ drop (playerIndex + 1) playerList

--------------------------------------------------------------
-------------- All Actions a player can make -----------------
-- All actions end by passing the turn
performAction :: Action -> Int -> State Table ()
performAction action playerPos = do 
    case action of
        Check   -> check playerPos
        Fold    -> fold playerPos
        Call    -> call playerPos
        Raise x -> raise playerPos x
        AllIn   -> allIn playerPos
    playerHaveActed playerPos

playerHaveActed :: Int -> State Table ()
playerHaveActed playerPos = do
    table <- get
    let players'  = players table
        player   = (players'!!playerPos) {hasActed = True}
        players'' = replacePlayer playerPos player players'
    put table
        { players = players''}
--------------------------------------------------------------

-- | Change a players fold-status to True
fold :: Int -> State Table ()
fold playerPos = do
    table <- get
    let player   = players table!!playerPos
        player'  = player {folded = True}
        players' = replacePlayer playerPos player' (players table)
    put table 
        { players = players'}
    

---------------------------------------    
-- | Pass the turn to the next player -- Might not need this
check :: Int -> State Table ()
check playerPos = do return ()
    -- table <- get
    -- let player   = (players table!!playerPos) {hasActed = True}
    --     players' = replacePlayer playerPos player (players table)
    -- put table {players = players'}

    
---------------------------------------    

-- | Take an int for how much to raise, then adds the lowest bet
raise :: Int -> Bet -> State Table ()
raise playerPos raiseamout = do
    table <- get
    let player = players table!!playerPos
        bet = raiseamout + lowestBet table player
    placeBet playerPos bet
    

---------------------------------------
-- | A player bet the lowest amount they can to get to next phase
call :: Int -> State Table ()
call playerPos = do
    table <- get
    let player = players table!!playerPos
        lowBet = lowestBet table player
    placeBet playerPos lowBet
    

---------------------------------------
-- | Bet all chips
allIn :: Int -> State Table ()
allIn playerPos = do
    table <- get
    let player = players table!!playerPos
    placeBet playerPos (chips player)
    
--------------------------------------------------------------
--------------------------------------------------------------

--------------------------------------------------------------
--------------------------------------------------------------
    
-- | Pure function, decrease the amount of chips a player have by a certain amount
decChips :: Player -> Bet -> Player
decChips player bet = player {chips         = (chips player)-bet,
                              commitedChips = (commitedChips player)+bet}

-- | increase the amount of chips a player have by a certain amount
incChips :: Player -> Pot -> Player
incChips player pot = player {chips = (chips player)+pot}

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


--------------------------------------------------------------
--------------------------------------------------------------
-- | Manual testing:
player1 :: Player
player1 = Player "Axel" hand1 400 0 True False NoBlind False

player2 :: Player
player2 = Player "Frodo" hand2 340 100 False False NoBlind False

player3 :: Player
player3 = Player "Sam" hand3 530 0 False False NoBlind False

playerlist :: [Player]
playerlist = [player1, player2, player3]

table2 :: Table
table2 = Table playerlist{- playerlist -}100 [] fullDeck [] PreFlop 200 0 1 2