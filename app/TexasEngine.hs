module TexasEngine where

import Types
import Cards
import Actions
import Control.Monad.State
import Data.Char (toLower)
import System.Random
import HandEvaluation
import Utilities

------------------------------------------------------------
------------------------------------------------------------


-- https://cstml.github.io/2021/07/22/State-Monad.html

gameRound :: Game ()
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
{-
    moveToNextPhase
    dealCommunityCards
    bettingRound

    -- after this we need to do showdown and check winenr hand combination etc.

-}

--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
-------------- All Game () - Functions -----------------------
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


--------------------------------------------------------------    
--------------------------------------------------------------

        
--------------------------------------------------------------
------------ Functions to print out sepcific things ----------

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


--------------------------------------------------------------
--------------------------------------------------------------
runShuffle2 :: Game()
runShuffle2 = do
    gen <- newStdGen
    let (doubles, _) = randomDoubles' 52 gen
    let shuffledDeck = shuffle doubles fullDeck
    modify(\table -> table {deck = shuffledDeck})

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

-- | Reset the required fields in the table and the players in the table.
resetGameState :: State Table ()--Game ()
resetGameState = 
    modify (\table -> 
        let resetPlayer player = player 
                { folded = False,
                  checked = False,
                  commitedChips = 0,
                  hasActed = False
                }
            resetPlayer' = map resetPlayer (players table)
        
        in table 
            { players = resetPlayer',
              --activePlayers = resetPlayer',
              highBet = 0,
              bets = []
            }
    )

-- | reset after a betting round
resetRound :: State Table ()
resetRound = 
    modify (\table -> 
        let resetPlayer player = player {commitedChips = 0, hasActed = False}
            resetPlayer' = map resetPlayer (players table)
        
        in table 
            { players = resetPlayer',
              highBet = 0
            }
    )    


--------- Dealing of cards, hands, communitycards -----------

-- | Initialisation of the blinds of the game.
initiateBlinds :: State Table ()
initiateBlinds = do
    table <- get

    let sbPlayer = (smallBlindPosition table)
        bbPlayer = (bigBlindPosition table)

    placeBet sbPlayer 50
    placeBet bbPlayer 100
 -- We can later on introduce logic that increases blind amount per round

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
    --table <- get
    playerList <- gets players

    playerList' <- mapM (\player -> do
        deltHand <- dealCards 2
        return player { hand = deltHand}
        ) playerList
    modify (\table -> table {players = playerList'{-, activePlayers = playerList'-}})


-- | Deal community cards to the board.
dealCommunityCards :: State Table ()--Game ()
dealCommunityCards = do
    table <- get
    let currentPhase = phase table
    cards <- case currentPhase of
        Flop -> dealCards 3
        Turn -> dealCards 1
        River -> dealCards 1
        _ -> return []

    modify (\table -> table { board = board table ++ cards }) 

-- | Move where the dealer,SB, and BB are on the table. Mod helps us wrap around.
moveDealer :: State Table ()
moveDealer = do
    table <- get
    let numPlayers = length (players table)
        newDealerPosition     = ((dealerPosition table) + 1) `mod` numPlayers
        newSmallBlindPosition = ((newDealerPosition) + 1) `mod` numPlayers
        newBigBlindPosition   = ((newSmallBlindPosition) + 1) `mod` numPlayers

    modify (\table -> table 
        { dealerPosition     = newDealerPosition,
          smallBlindPosition = newSmallBlindPosition,
          bigBlindPosition   = newBigBlindPosition
        })


-- Maybe a away to do the showdown... simple way...
showdown :: State Table [Int]
showdown = do
    table <- get
    let players'  = filterFolded (players table)
        communityCards = board table
        hands    = [hand player | player <- players']
        winners'  = winners communityCards hands
        chips    = div (pot table) (length winners')
        players'' = dealOutChips players' winners' chips
    put table {players = players'', pot = 0}
    return winners'


 -- | Advance from the tables current phase to the next one.
moveToNextPhase :: State Table () --Game ()
moveToNextPhase = modify (\table -> table { phase = nextPhase (phase table) })   
--------------------------------------------------------------
