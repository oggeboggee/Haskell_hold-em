module Actions where

{- Logic for the the different actions a player can make -}

import Types

import Control.Monad.State
import Data.Char
import Data.List (isPrefixOf)
import Utilities
import HandEvaluation
import Cards

-- With these changes the gameloop for eventsshould become
-- 1. Pick a player
-- 2. Get input from player with IO
-- 3. Convert the input into an Event
-- 4. Apply the event to the table (the logci)
-- 5. As a result get the new table and the event(s) (table', [GameEvent])
-- 6. Print the resulting action with IO (eventMSg)
-- 7. Repeat loop

-- How do we separate events driven by the engine and driven by the player?
-- Maybe move all game-type - functions to one file? 

-- change getPlayerAction and convertAction to sue Event instead of Action?
-- | Show a player what actions they can make (based on highBet), prompt player to type desired action,
--   take action and convert it from a string into what happens in-game, prompt user if action is wrong.
getPlayerEvent :: PlayerIndex -> Game Event
getPlayerEvent playerIndex = do
    table <- get

    let player = players table !! playerIndex
        hb     = highBet table
        lowbet = lowestBet table player

        availableActions = if hb == 0 || commitedChips player == hb
                           then "Fold, Check, Raise <x>, All In"
                           else "Fold, Call (" ++ show lowbet ++"), Raise <x>, All In"
    
    liftIO $ putStrLn $ "\n" ++ name player ++ " - Your available actions: " ++ availableActions 
                            ++ {-"\nCurrent high bet:    " ++ show (highBet table) ++-}
                               "\nCurrent pot:      " ++ show (pot table) ++    
                               "\nChips:          " ++ show (chips player) ++
                               "\nCommited chips: " ++ show (commitedChips player)
    printHand player

    input <- liftIO getLine

    case convertAction input of
        Just action -> return (PlayerEvent playerIndex action)
        Nothing -> do
            liftIO $ putStrLn "Input isn't valid."
            getPlayerEvent playerIndex


-- | Take the user input and convert it into a Maybe Action.
convertAction :: String -> Maybe Action
convertAction userInput = 
    let s = map toLower userInput
    in case s of
        "fold" -> Just Fold
        "check" -> Just Check
        "call" -> Just Call
        "all in" -> Just AllIn
        "allin" -> Just AllIn
        _ | "raise " `isPrefixOf` s -> if all isDigit amount then Just (Raise (read amount)) else Nothing
                                        where amount = drop 6 s
        _ -> Nothing

-- | Source guide:
-- | https://www.ahri.net/2019/07/practical-event-driven-and-sourced-programs-in-haskell/
-- | Here we update the state, have betting rules (when can a player call, raise etc.). 
-- | We handle errors if wanted action isn't allowed. We produce GameEvents.
-- | This is instead of having applyPureAction.
applyEvent :: Event -> State Table (Either String [GameEvent])
--applyEvent :: Event -> Table -> Either String (Table, [GameEvent])

-- PLAYERACTIONS
applyEvent (PlayerEvent playerIndex action) = do
    table <- get
    let player = (players table) !! playerIndex
        hb     = (highBet table)
        cChips = (commitedChips player)
        plName = (name player)
        toCall = hb - cChips

    case action of
        Fold -> do
            modify (\t -> pureFold t playerIndex) 
            pure (Right [PlayerFolded plName])
        
        Check -> do
            if toCall == 0
            then do 
                modify (\t -> pureCheck t playerIndex)
                pure (Right [PlayerChecked plName])
            else
                pure (Left "You can't check when behind the highbet.")

        Call -> do
            if toCall <= 0
            then pure (Left "There isn't a bet to call.")
            else do

                let amount = hb - cChips
                modify (\t -> placePureBet t playerIndex amount)
                playerHaveActed playerIndex -- Change a players acted to True
                pure (Right [PlayerCalled plName amount])

                -- modify (\t -> placePureBet t playerIndex toCall)
                -- pure (Right [PlayerCalled plName toCall])


        Raise x -> do
            if x <= 0 || x > (chips player + lowestBet table player)
            then pure (Left "Raise amount must be larger than 0 and smaller then the amount of chips you have")
            else do

                let callAmount = hb - cChips
                    totalAmount = callAmount + x
                modify (\t -> placePureBet t playerIndex totalAmount)
                playerHaveActed playerIndex

                pure (Right [PlayerRaised plName x])

        AllIn -> do
            let amount = (chips player)

            modify (\t -> placePureBet t playerIndex amount)
            playerHaveActed playerIndex -- Change a players acted to True

            pure (Right [PlayerAllIn plName amount])

-- System Events
applyEvent (EngineEvent engineAction) = do
    table <- get
    case engineAction of
        PlaceBlind playerIndex blindType bet -> do
            let player = (players table) !! playerIndex
                plName = (name player)
            modify (\t -> placePureBet t playerIndex bet)
            pure (Right [PlayerPlacedBlinds plName blindType bet])


        -- Showdown_ -> do
        --     resultOfShowdown <- state (runState runShowDown)


        RunShowdown -> do
            resultOfShowdown <- runShowdown

            pure (Right resultOfShowdown)
            
runShowdown :: State Table [GameEvent]
runShowdown = do
    table <- get

    let playerList = (players table)
        finalBoard = (board table)

        -- only the players that are still in the hand
        -- ex. [Sam, Lewis]
        activePlayers = filter (not . folded) playerList

        -- ex. [[5H,8C], [7H,9H]]
        extractedHands = map hand activePlayers

        -- ex- [1]  , indexes relative to the activeplayers
        winnerIndexes = winners finalBoard extractedHands

        -- winners returns index of winner(s) in activePlayers list, need to map to who it is
        -- in activePlayers: ex. activePlayers !! 1 = Lewis
        computedWinners = map (\i -> activePlayers !! i) winnerIndexes
                
        potSize = (pot table)

        -- split the pot evenly
        evenShare =
            if null computedWinners
            then 0
            else potSize `div` length computedWinners

        winnerNames = map name computedWinners

        updatedPlayers = map (dealOutChips2 winnerNames evenShare) playerList

    modify (\t -> t { players = updatedPlayers, pot = 0 })
    pure [ShowdownHappened (map name computedWinners)]


-- | This is the function that prints the events.
-- | Should maybe be in TexasEnginge??
eventMsg :: GameEvent -> Game ()
eventMsg event = liftIO $ case event of
    PlayerFolded p -> putStrLn (p ++ " folds.")

    PlayerChecked p -> putStrLn (p ++ " checks.")

    PlayerCalled p amount -> putStrLn (p ++ " calls with " ++ (show amount) ++ " chips.")

    PlayerRaised p amount -> putStrLn (p ++ " raises with " ++ (show amount) ++ " chips.")

    PlayerAllIn p amount -> putStrLn (p ++ " goes all-in with " ++ (show amount) ++ " chips.")

    PlayerPlacedBlinds p blindType amount -> putStrLn (p ++ " placed the " ++ (show blindType) ++ " of " ++ (show amount) ++ " chips.")

    ShowdownHappened ps -> mapM_ (\p -> putStrLn (p ++ " wins!")) ps --Would be nice to see all active players cars at the end


-- | https://stackoverflow.com/questions/27609062/what-is-the-difference-between-mapm-and-mapm-in-haskell
-- | mapM_ : We can use this because we don't care about the result of eventMsg (We have Game () as return type.)
-- | We still print what we need. So mapM_ when we only care about the side effects?

-- | Here we update the state and trigger the output of eventMSg. 
-- | If we get Right returned from applyEvent, then we get a new table and a list of events that happened.
performEvent :: Event -> Game (Either String [GameEvent])
performEvent event = do
    table <- get
    
    let (result, newTable) = runState (applyEvent event) table
    
    put newTable

    case result of
        Left inv -> do
            liftIO $ putStrLn inv
            pure (Left inv)

        Right events -> do
            mapM_ eventMsg events
            pure (Right events)




-- | We need a function to take a player at a specific index in a list, 
--   and then replace with the updated player
-- replacePlayer :: PlayerIndex -> Player -> [Player] -> [Player]
-- replacePlayer playerIndex updatedPlayer playerList = 
--     take playerIndex playerList ++ [updatedPlayer] ++ drop (playerIndex + 1) playerList
updatePlayerAtIndex :: PlayerIndex -> (Player -> Player) -> Table -> Table
updatePlayerAtIndex playerIndex f table =
    let playerList = (players table)
        player = playerList !! playerIndex
        updatedPlayer = f player
        updatePlayerList = replacePlayer playerIndex updatedPlayer playerList
    in table { players = updatePlayerList }


-- placeBet that works with Gaem() 
--placeBet :: Int -> Bet -> State Table ()
placePureBet :: Table -> PlayerIndex -> Bet -> Table
placePureBet table playerIndex bet =
    let tableUpdated = updatePlayerAtIndex playerIndex 
                                (`decChips` bet) table
                        
        
        player = (players tableUpdated) !! playerIndex
    in tableUpdated
            { pot = incPot (pot tableUpdated) bet,
              bets = bet : (bets tableUpdated),
              highBet = max (highBet tableUpdated) (commitedChips player)
            }

pureFold :: Table -> PlayerIndex -> Table
pureFold table playerIndex =
    updatePlayerAtIndex playerIndex (\p -> p { folded = True }) table


pureCheck :: Table -> PlayerIndex -> Table
pureCheck table playerIndex = 
    updatePlayerAtIndex playerIndex (\p -> p { acted = True }) table


-- Change a players acted status to True
playerHaveActed :: Int -> State Table ()
playerHaveActed playerPos = do
    modify (updatePlayerAtIndex playerPos (\p -> p {acted = True}))


---------------------------------------    

{-
-- https://zvon.org/other/haskell/Outputprelude/read_f.html    

-}
printHand :: Player -> Game ()
printHand player = liftIO $ putStrLn ("Hand: " ++ show (hand player))


--------------------------------------------------------------
--------------------------------------------------------------



-- playersExampel1 :: [Player]
-- playersExampel1 = [Player "Bob" [] 1000 0 False False,
--                        Player "Sam" [] 1000 0 False False,
--                        Player "Jonathan" [] 1000 0 False False,
--                        Player "Lewis" [] 1000 0 False False]

-- table1 :: Table
-- table1 = Table 
--             { players = playersExampel1,
--               deck = fullDeck,
--               board = [],
--               phase = PreFlop,
--               highBet = 0,
--               pot = 0,
--               dealerPosition = 3,
--               smallBlindPosition = 0,
--               bigBlindPosition = 1,
--               bets = []
--             }