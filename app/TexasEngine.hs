module TexasEngine where

-- Other files
import Types
import Cards
import Actions
import HandEvaluation
import Utilities

-- Packages
import Control.Monad.State
import Control.Monad (void)
import Data.Char (toLower)
import System.Random


gameRound :: Game ()
gameRound = do

    liftIO $ putStrLn "\n--------------------------------"
    liftIO $ putStrLn "              NEW HAND             "
    liftIO $ putStrLn "--------------------------------"

    state $ runState resetTable
    state $ runState resetBettingRound
    initiateBlinds
    runShuffle
    state $ runState dealHands

    -- PREFLOP -- (We don't use the helper here because its a special case.)
    --printTable
    state $ runState moveToNextPhase
    table <- get
    --printPhase (phase table) table
    printPhase
    printBettingRound (firstPlayerToBet table)
    bettingRound

    -- FLOP --
    runPhase

    -- TURN --
    runPhase

    -- RIVER --
    runPhase

    -- SHOWDOWN --
    state $ runState moveToNextPhase
    printtHandsShowdown
    void $ performEvent (EngineEvent RunShowdown)

    -- NEXTHAND --
    --printTable
    newHand

-- | Runs a single phase of the game (Flop, Turn, or River)
-- | Handles moving to the next phase, dealing communitycards, resetting betting state,
-- | and initiatess a bettinground if possible.
runPhase :: Game ()
runPhase = do
    table <- get

    -- If only one player remains, the hand is already decided.
    if handOver table
        then pure ()
        else do
            state (runState moveToNextPhase)
            state (runState dealCommunityCards)
            state (runState resetBettingRound)

            table' <- get

            --printPhase (phase table) table
            printPhase
            
            -- Only start a bettinground if it makes sense, requires > 1 player with chips in hand.
            -- If everyone is allin, or only one player can act, we skip betting and move to next phase.
            if bettingRoundCanRun table'
                then do 
                    printBettingRound (firstPlayerToBet table')
                    bettingRound
                else 
                    pure ()


-------------- All Game () - Functions -----------------------
------------------------------------------------------------
-- https://cstml.github.io/2021/07/22/State-Monad.html

------------------------------------------------------------------
-------------- Bettinground, gameround, gameloop -----------------

-- Looping through the bettinground and making playeractions
gameLoop :: PlayerIndex -> Game ()
gameLoop playerIndex = do
    table <- get

    if bettingRoundOver table
        then return ()
    else do
        --printTable -- Only to see what happends with the state while working on buggs
        let playerList    = players table
            currentPlayer = playerList !! playerIndex

            inactive p = folded p || chips p == 0

        if inactive currentPlayer
            then gameLoop (nextPlayerToAct playerIndex playerList)
        else do
            event <- getPlayerEvent playerIndex

            result <- performEvent event

            case result of
                Left _ -> gameLoop playerIndex -- Same player retries

                Right _ -> gameLoop (nextPlayerToAct playerIndex playerList)


-- | Initiate the betting phase of the game.
bettingRound :: Game ()
bettingRound = do
    table <- get

    if bettingRoundOver table
        then return ()
        else gameLoop (firstPlayerToBet table)


newHand :: Game ()
newHand = do
    liftIO $ putStrLn "Deal next hand? (yes/no)"
    answer <- liftIO getLine
    case map toLower answer of
        "yes" -> do state $ runState moveToNextPhase
                    state $ runState moveDealer
                    --printBlinds
                    gameRound
        "no"  -> pure ()
        _     -> liftIO $ putStrLn "Invalid input"
--------------------------------------------------------------
--------------------------------------------------------------

-- | Return a shuffled deck with 52 cards
runShuffle :: Game()
runShuffle = do
    gen <- newStdGen
    let (doubles, _) = randomDoubles' 52 gen
    let shuffledDeck = shuffle doubles fullDeck
    modify (\table -> table {deck = shuffledDeck})

-- | Helperfunction to runShuffle, generate a list of random doubles
randomDoubles' :: Int -> StdGen -> ([Double], StdGen)
randomDoubles' 0 gen = ([], gen)
randomDoubles' n gen = (x:xs, gen2)
  where
    (x, gen1)  = randomR (0.0,1.0) gen
    (xs, gen2) = randomDoubles' (n-1) gen1


------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
--------------------------- Functions to modify the game state (table) -------------------
-- Jonathan
-- | Reset the required fields in the table and the players in the table.
resetTable :: State Table () -- Game ()
resetTable =
    modify (\t ->
        let resetPlayer p =
                p { folded = False,
                    acted = False,
                    commitedChips = 0
                  }

        in t
            { players = map resetPlayer (players t),
              highBet = 0,
              pot = 0,
              bets = [],
              board = []
            }
    )

-- | Reset after each betting round by clearing flags for players. Also resets relevant fields in the table
-- | but preserves the higBet during the PreFlop phase of the game (makes BB be highBet during PreFlop).
resetBettingRound :: State Table () -- Game ()
resetBettingRound = do
    table <- get

    let hb = case phase table of
                PreFlop -> highBet table
                _       -> 0

    modify (\t -> t { players = map (\p -> p { acted = False, commitedChips = 0 }) (players t), highBet = hb})


--------- Dealing of cards, hands, communitycards -----------

-- | Initialisation of the blinds of the game.
--initiateBlinds :: State Table ()
initiateBlinds :: Game ()
initiateBlinds = do
    sbPlayer <- gets smallBlindPosition
    bbPlayer <- gets bigBlindPosition


    -- performEvent (EngineEvent (PlaceBlind sbPlayer SmallBlind 50))
    -- performEvent (EngineEvent (PlaceBlind bbPlayer BigBlind 100))

    void $ performEvent (EngineEvent (PlaceBlind sbPlayer SmallBlind 50))
    void $ performEvent (EngineEvent (PlaceBlind bbPlayer BigBlind 100))



--------------------------------------------------------------
---------- Dealing of cards, hands, communitycards -----------

-- | Deal cards, update the deck after removing the cards
--   and return the cards that have been delt.
dealCards :: Int -> State Table [Card] --Game [Card] 
dealCards n = do
    table <- get
    let (deltCards, newDeck) = splitAt n (deck table)
    put table { deck = newDeck }
    return deltCards

-- | Deal hands to the players. Get the table and list of players, take the list of players
--   and for each player in the list, use 'dealCards' to take two cards from the deck. Put
--   the cards as the players hand, then put the list of players now with updated hands back
--   into the table.
dealHands :: State Table () --Game ()
dealHands = do

    playerList <- gets players

    playerList' <- mapM (\player -> do
        deltHand <- dealCards 2
        return player { hand = deltHand}
        ) playerList

    modify (\table -> table { players = playerList' })



-- | Deal community cards to the board.
dealCommunityCards :: State Table ()--Game () -- State Table ()
dealCommunityCards = do
    currentPhase <- gets phase
    cards <- case currentPhase of
        Flop  -> dealCards 3
        Turn  -> dealCards 1
        River -> dealCards 1
        _     -> return []

    modify (\t -> t { board = board t ++ cards })


--------------------------------------------------------------
------------------ Utility functions -------------------------



-- | Advance from the tables current phase to the next one.
moveToNextPhase :: State Table () -- Game ()
moveToNextPhase = modify (\t -> t { phase = nextPhase (phase t) })

-- | Move where the dealer,SB, and BB are on the table. Mod helps us wrap around.
-- | Rotates the delaer button around the table and updates SB and BB positions based on where the dealer
-- | button ends up.
moveDealer :: State Table ()
moveDealer = do
    table <- get
    let numPlayers = length (players table)

    if numPlayers == 2
        then do
            let newDealerPos = (dealerPosition table + 1) `mod` numPlayers
                newSBPos     = newDealerPos
                newBBPos     = (newDealerPos + 1) `mod` numPlayers

            modify (\t -> t
                { dealerPosition     = newDealerPos
                , smallBlindPosition = newSBPos
                , bigBlindPosition   = newBBPos
                })
        else do
            let newDealerPos = (dealerPosition table + 1) `mod` numPlayers
                newSBPos     = (newDealerPos + 1) `mod` numPlayers
                newBBPos     = (newDealerPos + 2) `mod` numPlayers

            modify (\t -> t
                { dealerPosition     = newDealerPos
                , smallBlindPosition = newSBPos
                , bigBlindPosition   = newBBPos
                })
    


--------------------------------------------------------------
------------ Functions to print out sepcific things ----------

-- | Printing helpers
-- printPhase :: GamePhase -> Table -> Game ()
-- printPhase phase table = 
--     liftIO $ do 
--         putStrLn ("\n")
--         putStrLn ("-------------------------")
--         putStrLn ((show phase) ++ " (Pot: " ++ show (pot table) ++ ")")
--         putStrLn ("-------------------------")
--         putStrLn ("\n")
--         --putStrLn (printTable table)
--         putStrLn ("\n")


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

-- printTable :: Table -> String
-- printTable table = 
--     let playersList = (players table)
--         d           = (dealerPosition table)
--         sb          = (smallBlindPosition table)
--         bb          = (bigBlindPosition table)

--         pos i 
--             | i == d = "(D)"
--             | i == sb = "(SB)"
--             | i == bb = "(BB)"
--             | otherwise = ""

--         showPlayer i p =
--             "chips: " ++ show (chips p)
--             ++ " | name: " ++ (name p)
--             ++ " | pos: " ++ pos i


--         indexedPlayers = zipWith showPlayer [0..] playersList

--         -- put each player on a new line
--         render [] = ""    
--         render [x] = x
--         render (x:xs) = x ++ "\n" ++ render xs

    -- in render indexedPlayers


printBettingRound :: PlayerIndex -> Game ()
printBettingRound playerPos = do
    table <- get
    case phase table of
        Showdown -> return ()
        _        -> liftIO $ putStrLn $ "First player to act: "
                            ++ name (players table!!playerPos)

--Axel

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

--------------------------------------------------------------



-- | Manual testing:
player1 :: Player
player1 = Player "Axel" hd11 400 0 False False

player2 :: Player
player2 = Player "Frodo" hd12 340 100 False False

player3 :: Player
player3 = Player "Sam" hd13 530 0 False False

playerlist :: [Player]
playerlist = [player1, player2, player3]

table2 :: Table
table2 = Table playerlist{- playerlist -}100 [] fullDeck [] PreFlop 200 0 1 2

hd11 :: Hand
hd11 = [ Card Two Hearts, Card Jack Spades]

hd12 :: Hand
hd12 = [ Card Two Diamonds, Card Jack Spades]

hd13 :: Hand
hd13 = [ Card Ten Spades, Card King Clubs]