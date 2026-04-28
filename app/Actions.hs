module Actions where

{- Logic for the the different actions a player can make -}

import Types

import Control.Monad.State
import Data.Char
import Data.List (isPrefixOf)
import Utilities


-- With these changes the gameloop for eventsshould become
-- 1. Pick a player
-- 2. Get input from player with IO
-- 3. Convert the input into an Event
-- 4. Apply the event to the table (the logci)
-- 5. As a result get the new table and the event(s) (table', [GameEvent])
-- 6. Print the resulting action with IO (eventMSg)
-- 7. Repeat loop

-- How do we separate events driven by the engine and driven by the player?

-- change getPlayerAction and convertAction to sue Event instead of Action?
-- | Show a player what actions they can make (based on highBet), prompt player to type desired action,
--   take action and convert it from a string into what happens in-game, prompt user if action is wrong.
getPlayerEvent :: PlayerIndex -> Game Event
getPlayerEvent playerIndex = do
    table <- get

    let player = (players table) !! playerIndex
        hb     = (highBet table)

        availableActions = if hb == 0
                           then "Fold, Check, Raise <x>, All In"
                           else "Fold, Call, Raise <x>, All In"
    
    liftIO $ putStrLn ((name player) ++ " - Your available actions: " ++ availableActions)
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



-- | Source guide:
-- | https://www.ahri.net/2019/07/practical-event-driven-and-sourced-programs-in-haskell/
-- | Here we update the state, have betting rules (when can a player call, raise etc.). 
-- | We handle errors if wanted action isn't allowed. We produce GameEvents.
-- | This is instead of having applyPureAction.
-- applyEvent :: Event -> State Table (Either String [GameEvent])
applyEvent :: Event -> Table -> Either String (Table, [GameEvent])

-- PLAYERACTIONS
applyEvent (PlayerEvent playerIndex action) table =
    let player = (players table) !! playerIndex
        hb     = (highBet table)
        cChips = (commitedChips player)
        plName = (name player)
    in case action of
        Fold -> let table' = pureFold table playerIndex
                in Right (table', [PlayerFolded plName])
        
        Check -> if cChips == hb
                 then 
                    let table' = pureCheck table playerIndex
                    in Right (table', [PlayerChecked plName])
                 else Left "You can't check when behind the highbet."

        Call -> if hb == 0
                then Left "There isn't a bet to call."
                else
                    let amount = hb - cChips
                        table' = placePureBet table playerIndex amount
                    in Right (table', [PlayerCalled plName amount])

        Raise x -> if x <= 0
                   then Left "Raise amount must be larger than 0."
                   else
                    let callAmount = hb - cChips
                        callAndRaiseAmount = callAmount + x
                        table' = placePureBet table playerIndex callAndRaiseAmount
                    in Right (table', [PlayerRaised plName x])

        AllIn -> let amount = (chips player)
                     table' = placePureBet table playerIndex amount
                 in Right (table', [PlayerAllIn plName amount])

-- System Events
applyEvent (SystemEvent systemAction) table =
    case systemAction of
        PlaceBlind playerIndex blindType bet ->
            let table' = placePureBet table playerIndex bet
                player = (players table) !! playerIndex
                plName = (name player)
            in
                Right (table', [PlayerPlacedBlinds plName blindType bet])

-- | This is the function that prints the events.
eventMsg :: GameEvent -> Game ()
eventMsg event = liftIO $ case event of
    PlayerFolded p -> putStrLn (p ++ " folds.")

    PlayerChecked p -> putStrLn (p ++ " checks.")

    PlayerCalled p amount -> putStrLn (p ++ " calls with " ++ (show amount) ++ " chips.")

    PlayerRaised p amount -> putStrLn (p ++ " raises with " ++ (show amount) ++ " chips.")

    PlayerAllIn p amount -> putStrLn (p ++ " goes all-in with " ++ (show amount) ++ " chips.")

    PlayerPlacedBlinds p blindType amount -> putStrLn (p ++ " placed the " ++ (show blindType) ++ " of " ++ (show amount) ++ " chips.")


-- | https://stackoverflow.com/questions/27609062/what-is-the-difference-between-mapm-and-mapm-in-haskell
-- | mapM_ : We can use this because we don't care about the result of eventMsg (We have Game () as return type.)
-- | We still print what we need. So mapM_ when we only care about the side effects?

-- | Here we update the state and trigger the output of eventMSg. 
-- | If we get Right returned from applyEvent, then we get a new table and a list of events that happened.
performEvent :: Event -> Game ()
performEvent event = do
    table <- get
    case applyEvent event table of
        Left inv -> liftIO $ putStrLn inv

        Right (table', gEvents) -> do
            put table'
            mapM_ eventMsg gEvents

-- | We need a function to take a player at a specific index in a list, 
--   and then replace with the updated player
replacePlayer :: PlayerIndex -> Player -> [Player] -> [Player]
replacePlayer playerIndex updatedPlayer playerList = 
    take playerIndex playerList ++ [updatedPlayer] ++ drop (playerIndex + 1) playerList



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
                        (\player -> decChips player bet) table
        
        player = (players tableUpdated) !! playerIndex

pureFold :: Table -> PlayerIndex -> Table
pureFold table playerIndex =
    updatePlayerAtIndex playerIndex (\p -> p { folded = True }) table


pureCheck :: Table -> PlayerIndex -> Table
pureCheck table playerIndex = 
    updatePlayerAtIndex playerIndex (\p -> p { acted = True }) table

---- AXEL
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

    
    in tableUpdated
            { pot = incPot (pot tableUpdated) bet,
              bets = bet : (bets tableUpdated),
              highBet = max (highBet tableUpdated) (commitedChips player)
            }

---------------------------------------    
-- | Pass the turn to the next player -- Might not need this
check :: Int -> State Table ()
check playerPos = do return ()


-- https://zvon.org/other/haskell/Outputprelude/read_f.html    


printHand :: Player -> Game ()
printHand player = liftIO $ putStrLn ("Hand: " ++ show (hand player))

{-
--------------------------------------------------------------
--------------------------------------------------------------
-- | Manual testing:
player1 :: Player
player1 = Player "Axel" hand1 400 0 False False NoBlind False

player2 :: Player
player2 = Player "Frodo" hand2 340 100 False False NoBlind False

player3 :: Player
player3 = Player "Sam" hand3 530 0 False False NoBlind False

playerlist :: [Player]
playerlist = [player1, player2, player3]

table2 :: Table
table2 = Table playerlist{- playerlist -}100 [] fullDeck [] PreFlop 200 0 1 2

-}