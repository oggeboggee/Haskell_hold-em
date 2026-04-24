module TexasEngine where

import Types
import Cards
import Actions
import Control.Monad.State
import Data.Char (toLower)
import System.Random
import HandEvaluation

------------------------------------------------------------
------------------------------------------------------------
-- | Progress the phase to the next phase
nextPhase :: GamePhase -> GamePhase
nextPhase p = case p of
    DealHands   -> PreFlop
    PreFlop     -> Flop
    Flop        -> Turn
    Turn        -> River
    River       -> Showdown
    Showdown    -> DealHands


-- | Remake with StateT IO



-- https://cstml.github.io/2021/07/22/State-Monad.html

gameRound :: Game ()
gameRound = do
    liftIO $ putStrLn "Game is starting.."
    resetGameState

    liftIO $ putStrLn "Initiating blinds.."
    state (runState initiateBlinds)

    runShuffle2
    liftIO $ putStrLn "Dealing hands.."
    dealHands
    -- PreFlop
    moveToNextPhase
    table <- get
    printTable

    bettingRound (firstPlayerToBet table)
    printTable
    
    -- Flop
    moveToNextPhase
    resetGameState
    dealCommunityCards
    printTable

    bettingRound (firstPlayerToBet table)
    printTable

    -- Turn
    moveToNextPhase
    resetGameState
    dealCommunityCards
    printTable

    bettingRound (firstPlayerToBet table)
    printTable

    -- River
    moveToNextPhase
    resetGameState
    dealCommunityCards
    printTable

    bettingRound (firstPlayerToBet table)
    printTable
    
    -- Showdown
    moveToNextPhase
    win <- state (runState showdown)
    liftIO (showWinners win)
    -- printTable
{-
    moveToNextPhase
    dealCommunityCards
    bettingRound

    -- after this we need to do showdown and check winenr hand combination etc.

-}



{-
bettingRound :: Game ()
bettingRound = do
    resetCheckedPlayers    -- Need to make something to reset players, no fold,check.
    table <- get
    let initiate = (firstPlayerToBet table)
-}


------------------------------------------------------------------------------------------
------------ Code for each step of the game ---------------------------------------------


-- | Reset the required fields in the table and the players in the table.
resetGameState :: Game ()
resetGameState = 
    modify (\table -> 
        let resetPlayer player = player 
                { folded = False,
                  checked = False,
                  commitedChips = 0
                }
            resetPlayer' = map resetPlayer (players table)
        
        in table 
            { players = resetPlayer',
              activePlayers = resetPlayer',
              highBet = 0,
              bets = []
            }
    )    


-- | Reset players who have checked their hands
resetCheckedPlayers :: Game ()
resetCheckedPlayers = do
    table <- get
    let uncheckPlayers = map (\players -> players {checked = False}) (players table)

    modify (\table -> table { players = uncheckPlayers})


-- | Initialisation of the blinds of the game.
initiateBlinds :: State Table ()
initiateBlinds = do
    table <- get

    let sbPlayer = (smallBlindPosition table)
        bbPlayer = (bigBlindPosition table)

    placeBet sbPlayer 50
    placeBet bbPlayer 100
 -- We can later on introduce logic that increases blind amount per round

-- Take initiateBlinds and return a Game () type
initiateBlindsIO :: Game ()
initiateBlindsIO = do
    state (runState initiateBlinds)
    

---------- Dealing of cards, hands, communitycards -----------

-- | Deal cards, update the deck after removing the cards
--   and return the cards that have been delt.
dealCards :: Int -> Game [Card]
dealCards n = do
    table <- get
    let (deltCards, newDeck) = splitAt n (deck table)
    put table { deck = newDeck }
    return deltCards

-- | Deal hands to the players. Get the table and list of players, take the list of players
--   and for each player in the list, use 'dealCards' to take two cards from the deck. Put
--   the cards as the players hand, then put the list of players now with updated hands back
--   into the table.
dealHands :: Game ()
dealHands = do
    table <- get
    playerList <- gets players

    playerList' <- mapM (\player -> do
        deltHand <- dealCards 2
        return player { hand = deltHand}
        ) playerList
    modify (\table -> table {players = playerList', activePlayers = playerList'})


-- | Deal community cards to the board.
dealCommunityCards :: Game ()
dealCommunityCards = do
    table <- get
    let currentPhase = phase table
    cards <- case currentPhase of
        Flop -> dealCards 3
        Turn -> dealCards 1
        River -> dealCards 1
        _ -> return []

    modify (\table -> table { board = board table ++ cards }) 


roundOver :: Table -> Bool
roundOver table = 
    let playerList' = (activePlayers table)
        hb          = (highBet table)   
    in  length playerList' == 1 
        || all (\p -> (commitedChips p) == hb || checked p) playerList'



-- | Who bets first varies depedning on if we are in PreFlop state or any other state. PreFlop it is
--   the player UTG (BB+1). In all postflop betting rounds action begins with the player in the SB position.
firstPlayerToBet :: Table -> Int
firstPlayerToBet table = case phase table of
    PreFlop -> nextPlayerToAct (bigBlindPosition table) (activePlayers table)
    _       -> (smallBlindPosition table)



--startOfGame :: Table -> Bool
--startOfGame table | (table phase) == DealHands = True
--                  | (table phase) == _         = False



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



-- | Decided the next player to (we can ONLY use activeplayers here. Should we maybe have a specific type
--   that guarantees it will work only for activePlayers list?)
nextPlayerToAct :: Int -> [Player] -> Int
nextPlayerToAct i activePlayers = nextActivePlayer
    where nextActivePlayer = (i + 1) `mod` (length activePlayers)


    



--------------------------------------------------------------


                                
-- | Advance from the tables current phase to the next one.
moveToNextPhase :: Game ()
moveToNextPhase = modify (\table -> table { phase = nextPhase (phase table) })





-- Gameloop for Game ().
gameLoop :: Game ()
gameLoop = do
    gameRound
    --gameLoop



printTable :: Game ()
printTable = do
    table <- get
    liftIO $ print table
    --liftIO $ putStrLn ("Current table: " ++ show table)





--------------------------------------------------------------
--------------------------------------------------------------
--------------------------------------------------------------
chooseAction :: Int -> Game ()
chooseAction playerPos = do
    table <- get
    let playerToAct = players table!!playerPos
    liftIO $ putStrLn $ "Available actions: " ++ availableActions table
    liftIO $ putStrLn $ name playerToAct ++ ", choose action: "
    input <- liftIO getLine
    let s = (map toLower) input
    case s of
        "fold"  -> state $ runState (performAction Fold playerPos)
        "call"  -> state $ runState (performAction Call playerPos)
        "check" -> state $ runState (performAction Check playerPos)
        "raise" -> do 
            liftIO $ putStrLn "Type in amount: " -- Crashes if no number is typen in
            amount <- liftIO readLn  
            state $ runState (performAction (Raise amount) playerPos)
        "allin" -> state $ runState (performAction AllIn playerPos)
        _       -> do 
            liftIO $ putStrLn "Invalid option"
            chooseAction playerPos


--------------------------------------------------------------    
canCheck :: Table -> Bool
canCheck table = highBet table == 0

availableActions :: Table -> String
availableActions table = if canCheck table then "Check, " ++ actions
                         else "Call, " ++ actions
    where
        actions = "Fold, Raise, AllIn"

--------------------------------------------------------------
bettingRound :: Int -> Game ()
bettingRound playerToAct = do
    table <- get
    if roundOver2 (players table) (highBet table) 
        then do 
            return ()
    else do
        chooseAction playerToAct
        printTable
        bettingRound (nextPlayerToAct playerToAct (players table))
        
--------------------------------------------------------------
--------------------------------------------------------------

-- Maybe a away to do the showdown... simple way...
showdown :: State Table [Player]
showdown = do
    table <- get
    let players'  = players table
        communityCards = board table
        hands    = [hand player | player <- players table]
        winners'  = winners communityCards hands
        chips    = div (pot table) (length winners')
        players'' = dealOutChips players' winners' chips
    put table {players = players''}
    return players''

showWinners :: [Player] -> IO ()
showWinners ps = do
    let names      = [name player | player <- ps]
        nameString = printNames names
    putStrLn $ "The winners are: " ++ nameString


printNames :: [String] -> String
printNames []     = ""
printNames (x:xs) = " " ++ x ++ ", "



dealOutChips :: [Player] -> [Int] -> Int -> [Player]
dealOutChips players []      _     = players
dealOutChips players (x:xs)  chips = dealOutChips players' indexes' chips
    where
        indexes' = xs
        player   = incChips (players!!x) chips
        players' = replacePlayer x player players


--------------------------------------------------------------
roundOver2 :: [Player] -> Int -> Bool
roundOver2 players highBet = and [matchHBet p highBet || hasFolded p | p <- players]

matchHBet :: Player -> Int -> Bool
matchHBet player highBet = commitedChips player == highBet && highBet /= 0

hasFolded :: Player -> Bool
hasFolded = folded 


--------------------------------------------------------------
--------------------------------------------------------------
runShuffle2 :: Game()
runShuffle2 = do
    gen <- newStdGen
    let (doubles, _) = randomDoubles' 52 gen
    let shuffledDeck = (shuffle doubles fullDeck)
    modify(\table -> table {deck = shuffledDeck})

randomDoubles' :: Int -> StdGen -> ([Double], StdGen)
randomDoubles' 0 gen = ([], gen)
randomDoubles' n gen = ((x:xs), gen2)
  where
    (x, gen1)  = randomR (0.0,1.0) gen
    (xs, gen2) = randomDoubles' (n-1) gen1

