module TexasEngine where

import Types
import Cards
import Actions
import Control.Monad.State
import Data.Char (toLower)
import System.Random
import HandEvaluation
import Utilities
import Control.Monad (void)

-- Jonathan
gameRound :: Game ()
gameRound = do
    
    liftIO $ putStrLn "\n--------------------------------"
    liftIO $ putStrLn "              NEW HAND             "
    liftIO $ putStrLn "--------------------------------"
    
    resetTable
    resetBettingRound
    initiateBlinds
    dealHands

    -- PREFLOP -- (We don't use the helper here because its a special case.)
    table <- get
    printPhase (phase table) table
    printBettingRound (firstPlayerToBet table)
    bettingRound

    -- FLOP --
    runPhase 

    -- TURN --
    runPhase 

    -- RIVER --
    runPhase

    -- SHOWDOWN --
    void $ performEvent (EngineEvent RunShowdown)

-- | Above is quite repetetive so I'll make a helper function to make it shorter and cleaner
runPhase :: Game ()
runPhase = do
    moveToNextPhase
    dealCommunityCards
    resetBettingRound

    table <- get
    printPhase (phase table) table

    case phase table of
        Showdown -> pure ()
        _        -> do
                printBettingRound (firstPlayerToBet table)
                bettingRound





-- Axel
{-
gameRound2:: Game ()
gameRound = do

    liftIO $ putStrLn "Game is starting.."
    state (runState resetGameState)

    liftIO $ putStrLn "Initiating blinds.."
    state (runState initiateBlinds)

    runShuffle2
    liftIO $ putStrLn "Dealing hands.."
    state (runState dealHands)
    -- PreFlop
    state (runState moveToNextPhase)
    table <- get
    printTable
    liftIO $ putStrLn $ "First bettinground!" ++ show (phase table)
    bettingRound (nextPlayerToAct (bigBlindPosition table) (players table))
    printTable
    
    -- Flop
    liftIO $ putStrLn "Moving to next phase: Flop"
    state (runState moveToNextPhase)
    
    state (runState resetRound)
    state (runState dealCommunityCards)
    printTable
    liftIO $ putStrLn "Second bettinground!"
    bettingRound (nextPlayerToAct (dealerPosition table) (players table))
    printTable

    -- Turn
    liftIO $ putStrLn "Moving to next phase: Turn"
    state (runState moveToNextPhase)
    
    state (runState resetRound)
    state (runState dealCommunityCards)
    printTable
    liftIO $ putStrLn "Third bettinground!"
    bettingRound (nextPlayerToAct (dealerPosition table) (players table))
    printTable

    -- River
    liftIO $ putStrLn "Moving to next phase: River"
    state (runState moveToNextPhase)
    state (runState resetRound)
    state (runState dealCommunityCards)
    printTable
    liftIO $ putStrLn "Last bettinground!"
    bettingRound (nextPlayerToAct (dealerPosition table) (players table))
    printTable
    
    -- Showdown
    state (runState moveToNextPhase)
    win <- state (runState showdown)
    --liftIO $ print win
    showWinners win
    printTable
-}

-------------- All Game () - Functions -----------------------
------------------------------------------------------------
-- https://cstml.github.io/2021/07/22/State-Monad.html

------------------------------------------------------------------
-------------- Bettinground, gameround, gameloop -----------------
-- Jonathan
-- Looping through the bettinground and making playeractions
gameLoop :: PlayerIndex -> Game ()
gameLoop playerIndex = do
    table <- get

    if bettingRoundOver table
        then return ()
    else do

        let playerList    = (players table)
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
        else do
            gameLoop (firstPlayerToBet table)


--Axel
{-
-- Gameloop for Game ().
gameLoop :: Game ()
gameLoop = do
    gameRound

-- | Start the brtting round, recurisve with base case round over
--  Round over -> When all players have amtched the highest bet or folded and all have acted atleast once
bettingRound :: Int -> Game ()
bettingRound playerToAct = do
    table <- get
    if roundOver2 (players table) (highBet table) || onePlayerLeft (players table)
        then do
            return ()
    else do
        chooseAction playerToAct
        printTable
        bettingRound (nextPlayerToAct playerToAct (players table))

-- Axel
-- | Recive input from user and call for the correct actions depending on the input. 
chooseAction :: Int -> Game ()
chooseAction playerPos = do
    table <- get
    let playerToAct = players table!!playerPos
    liftIO $ putStrLn $ "Available actions: " ++ availableActions table playerPos
    liftIO $ putStrLn $ name playerToAct ++ ", choose action: "
    input <- liftIO getLine
    let s = (map toLower) input
    case s of
        "fold"  -> state $ runState (performAction Fold playerPos)
        "call"  -> state $ runState (performAction Call playerPos)
        "check" -> 
            if (highBet table == 0) || matchHBet playerToAct (highBet table)
                then state $ runState (performAction Check playerPos)
            else do liftIO $ putStrLn "Invalid option"
                    chooseAction playerPos
        "raise" -> do 
            liftIO $ putStrLn "Type in amount: " -- Crashes if no number is typen in
            amount <- liftIO readLn  
            state $ runState (performAction (Raise amount) playerPos)
        "allin" -> state $ runState (performAction AllIn playerPos)
        _       -> do 
            liftIO $ putStrLn "Invalid option"
            chooseAction playerPos

-}
--------------------------------------------------------------
--------------------------------------------------------------

-- | Return a shuffled deck with 52 cards
runShuffle2 :: Game()
runShuffle2 = do
    gen <- newStdGen
    let (doubles, _) = randomDoubles' 52 gen
    let shuffledDeck = shuffle doubles fullDeck
    modify(\table -> table {deck = shuffledDeck})

-- | Helperfunction to runShuffle, generate a list of random doubles
randomDoubles' :: Int -> StdGen -> ([Double], StdGen)
randomDoubles' 0 gen = ([], gen)
randomDoubles' n gen = ((x:xs), gen2)
  where
    (x, gen1)  = randomR (0.0,1.0) gen
    (xs, gen2) = randomDoubles' (n-1) gen1


------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
--------------------------- Functions to modify the game state (table) -------------------
-- Jonathan
-- | Reset the required fields in the table and the players in the table.
resetTable :: Game ()
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
resetBettingRound :: Game ()
resetBettingRound = do
    table <- get
    
    let hb = case (phase table) of
                PreFlop -> (highBet table) 
                _       -> 0
    
    modify (\t -> t { players = map (\p -> p { acted = False, commitedChips = 0 }) (players t), highBet = hb})

{-
-- Axel
-- | Reset the required fields in the table and the players in the table.
resetGameState :: State Table ()--Game ()
resetGameState = 
    modify (\table -> 
        let resetPlayer player = player 
                { folded        = False,
                  checked       = False,
                  commitedChips = 0,
                  hasActed      = False
                }
            resetPlayer' = map resetPlayer (players table)
        
        in table 
            { players = resetPlayer',
              highBet = 0,
              bets = []
            }
    )

-- | reset after a betting round
resetRound :: State Table ()
resetRound = 
    modify (\table -> 
        let resetPlayer player = player {commitedChips = 0, hasActed = False}
            resetPlayer'       = map resetPlayer (players table)
        
        in table 
            { players = resetPlayer',
              highBet = 0
            }
    )    
-}

--------- Dealing of cards, hands, communitycards -----------

-- | Initialisation of the blinds of the game.
--initiateBlinds :: State Table ()
initiateBlinds :: Game ()
initiateBlinds = do
    sbPlayer <- gets smallBlindPosition
    bbPlayer <- gets bigBlindPosition

    void $ performEvent (EngineEvent (PlaceBlind sbPlayer SmallBlind 50))
    void $ performEvent (EngineEvent (PlaceBlind bbPlayer BigBlind 100))

--AXEL
{-
--------------------------------------------------------------
---------- Dealing of cards, hands, communitycards -----------
initiateBlinds :: Game ()
initiateBlinds = do
    let sbPlayer = smallBlindPosition table
        bbPlayer = bigBlindPosition table

    placeBet sbPlayer 50
    placeBet bbPlayer 100
 -- We can later on introduce logic that increases blind amount per round

-}
-- | Deal cards, update the deck after removing the cards
--   and return the cards that have been delt.
dealCards :: Int -> Game [Card]  --State Table [Card] 
dealCards n = do
    table <- get
    let (deltCards, newDeck) = splitAt n (deck table)
    put table { deck = newDeck }
    return deltCards

-- | Deal hands to the players. Get the table and list of players, take the list of players
--   and for each player in the list, use 'dealCards' to take two cards from the deck. Put
--   the cards as the players hand, then put the list of players now with updated hands back
--   into the table.
dealHands :: Game () --State Table () 
dealHands = do

    playerList <- gets players

    playerList' <- mapM (\player -> do
        deltHand <- dealCards 2
        return player { hand = deltHand}
        ) playerList

    modify (\table -> table { players = playerList' })



-- | Deal community cards to the board.
dealCommunityCards :: Game () -- State Table ()
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
moveToNextPhase :: Game ()
moveToNextPhase = modify (\t -> t { phase = nextPhase (phase t) })

-- | Move where the dealer,SB, and BB are on the table. Mod helps us wrap around.
-- | Rotates the delaer button around the table and updates SB and BB positions based on where the dealer
-- | button ends up.
moveDealer :: State Table ()
moveDealer = do
    table <- get
    let numPlayers            = length (players table)
        newDealerPosition     = (dealerPosition table + 1) `mod` numPlayers
        newSmallBlindPosition = (newDealerPosition + 1) `mod` numPlayers
        newBigBlindPosition   = (newSmallBlindPosition + 1) `mod` numPlayers

    modify (\t -> t
        { dealerPosition     = newDealerPosition,
          smallBlindPosition = newSmallBlindPosition,
          bigBlindPosition   = newBigBlindPosition
        })

-- Axel
{-
-- Maybe a away to do the showdown... simple way...
showdown :: State Table [Int]
showdown = do
    table <- get
    let players'       = filterFolded (players table)
        communityCards = board table
        hands          = [hand player | player <- players']
        winners'       = winners communityCards hands
        chips          = div (pot table) (length winners')
        players''      = dealOutChips players' winners' chips
    put table {players = players'', pot = 0}
    return winners'

 -- | Advance from the tables current phase to the next one.
moveToNextPhase :: State Table () --Game ()
moveToNextPhase = modify (\table -> table { phase = nextPhase (phase table) })   
-}

--------------------------------------------------------------
------------ Functions to print out sepcific things ----------

-- | Printing helpers
printPhase :: GamePhase -> Table -> Game ()
printPhase phase table = 
    liftIO $ do 
        putStrLn ("\n")
        putStrLn ("-------------------------")
        putStrLn ((show phase) ++ " (Pot: " ++ show (pot table) ++ ")")
        putStrLn ("-------------------------")
        putStrLn ("\n")
        putStrLn (printTable table)
        putStrLn ("\n")

printTable :: Table -> String
printTable table = 
    let playersList = (players table)
        d           = (dealerPosition table)
        sb          = (smallBlindPosition table)
        bb          = (bigBlindPosition table)

        pos i 
            | i == d = "(D)"
            | i == sb = "(SB)"
            | i == bb = "(BB)"
            | otherwise = ""

        showPlayer i p =
            "chips: " ++ show (chips p)
            ++ " | name: " ++ (name p)
            ++ " | pos: " ++ pos i


        indexedPlayers = zipWith showPlayer [0..] playersList
    
        -- put each player on a new line
        render [] = ""    
        render [x] = x
        render (x:xs) = x ++ "\n" ++ render xs

    in render indexedPlayers


printBettingRound :: PlayerIndex -> Game ()
printBettingRound player = liftIO $ putStrLn ("First player to act: " ++ (show player))

--Axel
{-
printTable :: Game ()
printTable = do
    table <- get
    liftIO $ print table
    --liftIO $ putStrLn ("Current table: " ++ show table)

printCommunityCards :: Game ()
printCommunityCards = do
    table <- get
    liftIO $ print (board table)


showWinners :: [Int] -> Game ()
showWinners indexes = do
    table <- get
    let names      = [name (players table!!i) | i <- indexes]
        nameString = printNames names
    liftIO $ putStrLn $ "The winners are: " ++ nameString

-}


--------------------------------------------------------------
