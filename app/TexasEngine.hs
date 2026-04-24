module TexasEngine where

import Types
import Cards
import Actions
import Control.Monad.State

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
------------------------------------------------------------------
-------------- Bettinground, gameround, gameloop -----------------

-- Looping through the bettinground and making playeractions
gameLoop :: Int -> Bet -> Game ()
gameLoop playerIndex highestBet = do
    table <- get

    if bettingroundOver table   --- We need to make sure the logic for roundOVer works like we want it to.
    then do 
        liftIO $ putStrLn "Bettinground has finished, moving to next phase.." --how do we move out and to next hand dealing in gameRound
        moveToNextPhase

    else do 
        let currentPlayer = (players table) !! playerIndex
            playerList    = (players table)
        
        if (folded currentPlayer) || (chips currentPlayer) == 0
        then gameLoop (nextPlayerToAct playerIndex playerList) highestBet
        else do
            playerAction <- getPlayerAction playerIndex


            let printAction = case playerAction of
                    Fold -> (name currentPlayer) ++ " folds."
                    Check -> (name currentPlayer) ++ " checks."
                    Call -> (name currentPlayer) ++ " calls with (" ++ show (lowestBet table currentPlayer) ++ " chips.)"
                    AllIn -> (name currentPlayer) ++ " goes all-in with (" ++ show (chips currentPlayer) ++ " chips.)"
                    Raise x -> (name currentPlayer) ++ " raises with (" ++ show x ++ " chips.)"



            performAction2 playerAction playerIndex

            liftIO $ putStrLn printAction

            gameLoop (nextPlayerToAct playerIndex playerList) highestBet


gameRound :: Game ()
gameRound = do
    
    liftIO $ putStrLn "Game is starting.."
    resetGameState

    table <- get
    let playersList = (players table)
        sbIndex = (smallBlindPosition table)
        bbindex = (bigBlindPosition table)
        dindex = (dealerPosition table)

        sbPlayer = playersList !! sbIndex
        bbPlayer = playersList !! bbindex

    initiateBlinds
    liftIO $ putStrLn ((name sbPlayer) ++ ": placed the small blind | " ++ (name bbPlayer) ++ ": placed the big blind")
    table' <- get
    liftIO $ putStrLn (show table')

    dealHands
    liftIO $ putStrLn "Dealing hands.."
    bettingRound

    
    dealCommunityCards
    table''' <- get
    liftIO $ putStrLn "Flop delt.."
    showBoard
    bettingRound

    
    dealCommunityCards 
    liftIO $ putStrLn "River delt.."
    bettingRound

    
    dealCommunityCards
    liftIO $ putStrLn "Turn delt.."
    bettingRound

   

    -- after this we need to do showdown and check winenr hand combination etc.


showBoard :: Game ()
showBoard = do 
    board <- gets board
    liftIO $ print board


-- | Initiate the betting phase of the game.
bettingRound :: Game ()
bettingRound = do
    resetBettingRound   -- Need to make something to reset players, no fold,check.
    table <- get
    let firstPlayer = (firstPlayerToBet table)
        highestBet           = (highBet table)

    liftIO $ putStrLn ("Get ready to place your bets. First player to act: " ++ show firstPlayer)

    gameLoop firstPlayer highestBet





-------------------------------------------------------------------------------------------

-- | Initialisation of the blinds of the game.
--initiateBlinds :: State Table ()
initiateBlinds :: Game ()
initiateBlinds = do
    table <- get

    let sbPlayer = (smallBlindPosition table)
        bbPlayer = (bigBlindPosition table)

    placeBet sbPlayer 50
    placeBet bbPlayer 100


-- Take initiateBlinds and return a Game () type
--initiateBlindsIO :: Game ()
--initiateBlindsIO = do
--    state (runState initiateBlinds)
    
--------------------------------------------------------------
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
    modify (\table -> table { players = playerList' })


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



--------------------------------------------------------------
------------------ Utility functions -------------------------

-- | Reset the required fields in the table and the players in the table.
resetGameState :: Game ()
resetGameState = 
    modify (\table -> 
        let resetPlayer player = player 
                { folded = False,
                  checked = False
                }
            resetPlayer' = map resetPlayer (players table)
        
        in table 
            { players = resetPlayer',
              highBet = 0,
              bets = []
            }
    )

-- | Decided the next player to (we can ONLY use activeplayers here. Should we maybe have a specific type
--   that guarantees it will work only for activePlayers list?)
nextPlayerToAct :: Int -> [Player] -> Int
nextPlayerToAct i players = nextActivePlayer
    where nextActivePlayer = (i + 1) `mod` (length players)

                                
-- | Advance from the tables current phase to the next one.
moveToNextPhase :: Game ()
moveToNextPhase = modify (\table -> table { phase = nextPhase (phase table) })


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

-- | Who bets first varies depedning on if we are in PreFlop state or any other state. PreFlop it is
--   the player UTG (BB+1). In all postflop betting rounds action begins with the player in the SB position.
firstPlayerToBet :: Table -> Int
firstPlayerToBet table = case phase table of
    PreFlop -> nextPlayerToAct (bigBlindPosition table) (players table)
    _       -> (smallBlindPosition table)


bettingroundOver :: Table -> Bool
bettingroundOver table = 
    let playerList = (players table)
        hb          = (highBet table)

        allPlayersActed p =
            folded p ||
            chips p == 0 ||
            (commitedChips p == hb && acted p)
    in all allPlayersActed playerList   


-- | Reset players who have checked their hands
resetBettingRound :: Game ()
resetBettingRound = 
    modify (\table -> 
        table 
            { players = 
                map (\player -> player 
                    { checked = False,
                      acted = False
                    }
                ) (players table),
              highBet = 0
            })



-- remove below maybe --
printTable :: Game ()
printTable = do
    table <- get
    liftIO $ print table
    --liftIO $ putStrLn ("Current table: " ++ show table)


--startOfGame :: Table -> Bool
--startOfGame table | (table phase) == DealHands = True
--                  | (table phase) == _         = False
