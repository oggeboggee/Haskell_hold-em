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
    then return ()
    else do 
        let currentPlayer = (players table) !! playerIndex
            playerList    = (players table)
        
        if (folded currentPlayer) || (chips currentPlayer) == 0
        then gameLoop (nextPlayerToAct playerIndex playerList) highestBet
        else do
            
            playerAction <- getPlayerEvent playerIndex

            performEvent playerAction

            gameLoop (nextPlayerToAct playerIndex playerList) highestBet


gameRound :: Game ()
gameRound = do
    
    liftIO $ putStrLn "Game is starting.."
    resetTable

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

    -- PREFLOP
    dealHands
    liftIO $ putStrLn "Dealing hands.."
    moveToNextPhase
    bettingRound

    -- FLOP
    moveToNextPhase
    dealCommunityCards
    liftIO $ putStrLn "Flop delt.."
    showBoard
    bettingRound

    -- TURN
    dealCommunityCards 
    liftIO $ putStrLn "Turn delt.."
    bettingRound

    -- RIVER
    moveToNextPhase
    dealCommunityCards
    liftIO $ putStrLn "River delt.."
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
    sbPlayer <- gets smallBlindPosition
    bbPlayer <- gets bigBlindPosition

    performEvent (Blind sbPlayer 50)
    performEvent (Blind bbPlayer 100)

    
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

    playerList <- gets players

    playerList' <- mapM (\player -> do
        deltHand <- dealCards 2
        return player { hand = deltHand}
        ) playerList
    modify (\table -> table { players = playerList' })


-- | Deal community cards to the board.
dealCommunityCards :: Game ()
dealCommunityCards = do
    currentPhase <- gets phase
    cards <- case currentPhase of
        Flop -> dealCards 3
        Turn -> dealCards 1
        River -> dealCards 1
        _ -> return []

    modify (\t -> t { board = board t ++ cards }) 



--------------------------------------------------------------
------------------ Utility functions -------------------------

-- | Reset the required fields in the table and the players in the table.
resetTable :: Game ()
resetTable = 
    modify (\table -> 
        let resetPlayer p = 
                p { folded = False,
                    acted = False,
                    commitedChips = 0 
                  }
        
        in table 
            { players = map resetPlayer (players table),
              highBet = 0,
              pot = 0,
              bets = [],
              board = []
            }
    )

-- | Finds the next player in turn order who is eligible to act. I.e not folded and still has chips.
-- | It wraps around the table.
nextPlayerToAct :: Int -> [Player] -> Int
nextPlayerToAct i players = 
    let n = (length players)
        nextPlayer = (i + 1) `mod` n
        player = players !! nextPlayer
    in if ((folded player) || (chips player) == 0)
       then nextPlayerToAct nextPlayer players
       else nextPlayer 
                                
-- | Advance from the tables current phase to the next one.
moveToNextPhase :: Game ()
moveToNextPhase = modify (\table -> table { phase = nextPhase (phase table) })


-- | Rotates the delaer button around the table and updates SB and BB positions based on where the dealer
-- | button ends up.
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

-- | Determines wether the current betting round is over or not. A round is over once eveery player has
-- | either folded, is all-in, or matched the current highBet and taken an action.
bettingroundOver :: Table -> Bool
bettingroundOver table = 
    let playerList = (players table)
        hb         = (highBet table)

        allPlayersActed p =
            folded p ||
            chips p == 0 ||
            (commitedChips p == hb && acted p)
    in all allPlayersActed playerList   


-- | Reset after each betting round by clearing flags for players. Also resets relevant fields in the table
-- | but preserves the higBet during the PreFlop phase of the game (makes BB be highBet during PreFlop).
resetBettingRound :: Game ()
resetBettingRound = do
    table <- get
    
    let hb = case (phase table) of
                PreFlop -> (highBet table) 
                _       -> 0
    
    modify (\table -> 
        table 
            { players = 
                map (\player -> player { acted = False }) (players table),
                highBet = hb,
                bets = []
            })


-- remove below maybe --
printTable :: Game ()
printTable = do
    table <- get
    liftIO $ print table
    --liftIO $ putStrLn ("Current table: " ++ show table)


