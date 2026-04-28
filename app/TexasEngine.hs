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
    
    liftIO $ putStrLn "\n--------------------------------"
    liftIO $ putStrLn "              NEW HAND             "
    liftIO $ putStrLn "--------------------------------"
    
    resetTable
    resetBettingRound
    initiateBlinds
    dealHands

    -- PREFLOP --
    table <- get
    printPhase "PREFLOP" table
    printBettingRound (firstPlayerToBet table)
    bettingRound

    -- FLOP --
    moveToNextPhase
    dealCommunityCards
    resetBettingRound

    table <- get
    printPhase "FLOP" table
    printBettingRound (firstPlayerToBet table)
    bettingRound

    -- TURN --
    moveToNextPhase
    dealCommunityCards
    resetBettingRound

    table <- get 
    printPhase "TURN" table
    printBettingRound (firstPlayerToBet table)
    bettingRound

    -- RIVER --
    moveToNextPhase
    dealCommunityCards
    resetBettingRound

    table <- get
    printPhase "RIVER" table
    printBettingRound (firstPlayerToBet table)
    bettingRound

   

    -- after this we need to do showdown and check winenr hand combination etc.


-- | Initiate the betting phase of the game.
bettingRound :: Game ()
bettingRound = do
    table <- get
    let firstPlayer = (firstPlayerToBet table)
        highestBet           = (highBet table)

    gameLoop firstPlayer highestBet





-------------------------------------------------------------------------------------------

-- | Initialisation of the blinds of the game.
--initiateBlinds :: State Table ()
initiateBlinds :: Game ()
initiateBlinds = do
    sbPlayer <- gets smallBlindPosition
    bbPlayer <- gets bigBlindPosition

    performEvent (SystemEvent (PlaceBlind sbPlayer SmallBlind 50))
    performEvent (SystemEvent (PlaceBlind bbPlayer BigBlind 100))

    
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

-- | Finds the next player in turn order who is eligible to act. I.e not folded and still has chips.
-- | It wraps around the table.
nextPlayerToAct :: PlayerIndex -> [Player] -> PlayerIndex
nextPlayerToAct i playersList = 
    let n = (length playersList)
        nextPlayer = (i + 1) `mod` n
        player = playersList !! nextPlayer
    in if ((folded player) || (chips player) == 0)
       then nextPlayerToAct nextPlayer playersList
       else nextPlayer 
                                
-- | Advance from the tables current phase to the next one.
moveToNextPhase :: Game ()
moveToNextPhase = modify (\t -> t { phase = nextPhase (phase t) })


-- | Rotates the delaer button around the table and updates SB and BB positions based on where the dealer
-- | button ends up.
moveDealer :: State Table ()
moveDealer = do
    table <- get
    let numPlayers = length (players table)
        newDealerPosition     = ((dealerPosition table) + 1) `mod` numPlayers
        newSmallBlindPosition = ((newDealerPosition) + 1) `mod` numPlayers
        newBigBlindPosition   = ((newSmallBlindPosition) + 1) `mod` numPlayers

    modify (\t -> t
        { dealerPosition     = newDealerPosition,
          smallBlindPosition = newSmallBlindPosition,
          bigBlindPosition   = newBigBlindPosition
        })

-- | Who bets first varies depedning on if we are in PreFlop state or any other state. PreFlop it is
--   the player UTG (BB+1). In all postflop betting rounds action begins with the player in the SB position.
firstPlayerToBet :: Table -> PlayerIndex
firstPlayerToBet table = case phase table of
    PreFlop -> nextPlayerToAct (bigBlindPosition table) (players table)
    _       -> (smallBlindPosition table)

-- | Determines wether the current betting round is over or not. A round is over once eveery player has
-- | either folded, is all-in, or matched the current highBet and taken an action.
bettingroundOver :: Table -> Bool
bettingroundOver table = 
    let playerList = (players table)
        hb         = (highBet table)

    in all (\p ->
        folded p ||
        chips p == 0 ||
        commitedChips p == hb
        ) playerList 

-- | Reset after each betting round by clearing flags for players. Also resets relevant fields in the table
-- | but preserves the higBet during the PreFlop phase of the game (makes BB be highBet during PreFlop).
resetBettingRound :: Game ()
resetBettingRound = do
    table <- get
    
    let hb = case (phase table) of
                PreFlop -> (highBet table) 
                _       -> 0
    
    modify (\t -> t { players = map (\p -> p { acted = False, commitedChips = 0 }) (players t), highBet = hb})


-- | Printing helpers
printPhase :: String -> Table -> Game ()
printPhase phaseName table = 
    liftIO $ do 
        putStrLn ("\n")
        putStrLn ("-------------------------")
        putStrLn (phaseName ++ " (Pot: " ++ show (pot table) ++ ")")
        putStrLn ("-------------------------")
        putStrLn ("Board: " ++ show (board table))
        putStrLn ("\n")

printBettingRound :: PlayerIndex -> Game ()
printBettingRound player = liftIO $ putStrLn ("First player to act: " ++ (show player))



