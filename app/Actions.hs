module Actions where

{- Logic for the the different actions a player can make -}

import Types
import Cards ( fullDeck, hand1, hand2, hand3 )

import Control.Monad.State
import Data.Bits (Bits(xor))
import Data.Char
import Data.List (isPrefixOf)



-- | We need a function to take a player at a specific index in a list, 
--   and then replace with the updated player
replacePlayer :: Int -> Player -> [Player] -> [Player]
replacePlayer playerIndex updatedPlayer playerList = 
    take playerIndex playerList ++ [updatedPlayer] ++ drop (playerIndex + 1) playerList



-- We want to go: 
-- show actions based on what player can do -> user input -> convertAction -> applyAction -> update table?

-- | Show a player what actions they can make (based on highBet), prompt player to type desired action,
--   take action and convert it from a string into what happens in-game, prompt user if action is wrong.
getPlayerAction :: Int -> Game Action
getPlayerAction playerIndex = do
    table <- get

    let player = (players table) !! playerIndex
        hb = (highBet table)

        availableActions = if hb == 0
                           then "Fold, Check, Raise <x>, All In"
                           else "Fold, Call, Raise <x>, All In"
    
    liftIO $ putStrLn ((name player) ++ " - Your available actions: " ++ availableActions)

    input <- liftIO getLine

    case convertAction input of
        Just action -> return action
        Nothing -> do
            liftIO $ putStrLn "Input isn't valid."
            getPlayerAction playerIndex


-- | Take the user input and convert it into a Maybe Action.
convertAction :: String -> Maybe Action
convertAction userInput = 
    let s = (map toLower) userInput
    in case s of
        "fold" -> Just Fold
        "check" -> Just Check
        "call" -> Just Call
        "all in" -> Just AllIn
        "allin" -> Just AllIn
        _ | "raise " `isPrefixOf` s -> if all isDigit amount then Just (Raise (read amount)) else Nothing
                                        where amount = (drop 6 s) 
        _ -> Nothing



applyPureAction :: Table -> Action -> Int -> Table
applyPureAction table action playerIndex =
    case action of
        Fold -> pureFold table playerIndex

        Check -> if commitedChips ((players table) !! playerIndex) == (highBet table)
                 then pureCheck table playerIndex
                 else table --shouldnt be table
            
        Call -> if (highBet table) > 0
                then pureCall table playerIndex
                else table --shouldnt be table.

        Raise x -> pureRaise table playerIndex x

        AllIn -> pureAllIn table playerIndex


updatePlayerAtIndex :: Int -> (Player -> Player) -> Table -> Table
updatePlayerAtIndex playerIndex f table =
    let playerList = (players table)
        player = playerList !! playerIndex
        updatedPlayer = f player
        updatePlayerList = replacePlayer playerIndex updatedPlayer playerList
    in table { players = updatePlayerList }


-- placeBet that works with Gaem() 
--placeBet :: Int -> Bet -> State Table ()
placePureBet :: Table -> Int -> Bet -> Table
placePureBet table playerIndex bet =
    let tableUpdated = updatePlayerAtIndex playerIndex 
                        (\player -> player { chips = (chips player) - bet,
                                   commitedChips = (commitedChips player) + bet
                                }) table
        player = (players tableUpdated) !! playerIndex
    in tableUpdated
            { pot = (pot tableUpdated) + bet,
              bets = bet : (bets tableUpdated),
              highBet = max (highBet tableUpdated) (commitedChips player)
            }

placeBet :: Int -> Bet -> Game ()
placeBet playerIndex bet = modify (\table -> placePureBet table playerIndex bet)

pureFold :: Table -> Int -> Table
pureFold table playerIndex =
    updatePlayerAtIndex playerIndex (\p -> p { folded = True }) table


pureCheck :: Table -> Int -> Table
pureCheck table playerIndex = 
    updatePlayerAtIndex playerIndex (\p -> p { checked = True }) table


pureCall :: Table -> Int -> Table
pureCall table playerIndex =
    let player = (players table) !! playerIndex
        amount = max 0 (lowestBet table player)

    in placePureBet table playerIndex amount


pureRaise :: Table -> Int -> Bet -> Table
pureRaise table playerIndex raiseAmount =
    let player = (players table) !! playerIndex
        toCall = max 0 (lowestBet table player)
        amount = toCall + raiseAmount
    in placePureBet table playerIndex amount


pureAllIn :: Table -> Int -> Table
pureAllIn table playerIndex =
    let player = (players table) !! playerIndex
    in placePureBet table playerIndex (chips player)


performAction2 :: Action -> Int -> Game ()
performAction2 action playerIndex = do
    modify (\table -> applyPureAction table action playerIndex)

    modify (\table -> updatePlayerAtIndex playerIndex (\p -> p { acted = True }) table)


-- So, do we use pure -> StateT or pure -> State -> StateT?



-- https://zvon.org/other/haskell/Outputprelude/read_f.html    
--------------------------------------------------------------
--------------------------------------------------------------

--------------------------------------------------------------
--------------------------------------------------------------
    
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


--------------------------------------------------------------
--------------------------------------------------------------
-- | Manual testing:


--------------------------------------------------------------
-- Moved commented versions to here for now.

--------------------------------------------------------------
-------------- All Actions a player can make -----------------
-- All actions end by passing the turn
{-performAction :: Action -> Int -> Game ()
performAction action playerPos = do 
    case action of
        Check   -> check
        Fold    -> fold playerPos
        Call    -> call playerPos
        Raise x -> raise playerPos x
        AllIn   -> allIn playerPos

-}



--------------------------------------------------------------


    
{-
---------------------------------------    
-- | Pass the turn to the next player -- Might not need this
check :: State Table ()
check = do 
    table <- get
    put table
    
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
-}





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


-- | Change a players fold-status to True
{-
--fold :: Int -> State Table ()
fold :: Int -> Game()
fold playerPos = do
    table <- get
    let player   = players table!!playerPos
        players' = [if name p == name player 
                    then p {folded = True}
                    else p | p <- players table]
        active' = filter (not . folded) players'
    put table 
        { players = players',
          activePlayers = active'} --
-}           
{-
pureFold :: Table -> Int -> Table
pureFold table playerIndex =
    let allPlayers = (players table)
        currentPlayer = allPlayers !! playerIndex
        currentPlayer' = currentPlayer { folded = True }
        allPlayers' = replacePlayer playerIndex currentPlayer' allPlayers
    in table { players = allPlayers'}

fold :: Int -> Game ()
fold playerIndex = modify (\table -> pureFold table playerIndex)





-- placeBet that works with Gaem() 
--placeBet :: Int -> Bet -> State Table ()
placeBet :: Int -> Bet -> Game ()
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
              highBet = max (highBet table) (commitedChips updatedPlayer)
            }
        )



-}