module Engine.TerminalUI where

import Types.GameTypes
import Engine.Utilities

import Data.Char
import Data.List (isPrefixOf)

import Control.Monad.State

-- With these changes the gameloop for eventsshould become
-- 1. Pick a player
-- 2. Get input from player with IO
-- 3. Convert the input into an Event
-- 4. Apply the event to the table (the logci)
-- 5. As a result get the new table and the event(s) (table', [GameEvent])
-- 6. Print the resulting action with IO (eventMSg)
-- 7. Repeat loop


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
                            ++ {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-} {-"\nCurrent high bet:    " ++ show (highBet table) ++-}
                                {-"\nCurrent high bet:    " ++ show (highBet table) ++-}
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



---------------------------------------    
--------------------------------------------------------------
------------ Functions to print out sepcific things ----------

{-
-- https://zvon.org/other/haskell/Outputprelude/read_f.html    

-}
printHand :: Player -> Game ()
printHand player = liftIO $ putStrLn ("Hand: " ++ show (hand player))

printPhase :: Game ()
printPhase = do
    table <- get
    liftIO $ do
        putStrLn "\n"
        putStrLn "-------------------------"
        putStrLn $ show (phase table) ++ " (Pot: " ++ show (pot table) ++ ")"
        putStrLn $ "CommunityCards: " ++ show (board table)
        putStrLn "-------------------------"
        putStrLn "\n"
        --putStrLn (printTable table)
        putStrLn "\n"


printBettingRound :: PlayerIndex -> Game ()
printBettingRound playerPos = do
    table <- get
    case phase table of
        Showdown -> return ()
        _        -> liftIO $ putStrLn $ "First player to act: "
                            ++ name (players table!!playerPos)



printTable :: Game ()
printTable = do
    table <- get
    liftIO $ print table
    --liftIO $ putStrLn ("Current table: " ++ show table)


printBlinds :: Game ()
printBlinds = do
    table <- get
    let
        sb = smallBlindPosition table
        bb = bigBlindPosition table
        pl = players table

    liftIO $ putStrLn $ "sb: " ++ name (pl!!sb) ++ "\nbb: " ++ name (pl!!bb)

printtHandsShowdown :: Game ()
printtHandsShowdown = do
    table <- get
    let ps = filter (not . folded) (players table)
    if length ps <= 1 then return ()
    else do
        liftIO $ putStrLn "Showdown!\n"
        liftIO $ mapM_ (\p -> putStrLn (name p ++ "'s hand: " ++ show (hand p))) ps