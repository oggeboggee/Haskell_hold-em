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

------------------------------------------------------------ move this
-- | Change blind status to given blind
-- changeBlind :: Player -> Blind -> Player
-- changeBlind player blind = player {blind player = blind}

-- | Updating what happens during each phase (dealing cards, showing community cards, etc.) and moves phase to next phase
-- This is all hardcoded for now but will change later.
gameStep :: State Table ()
gameStep = do
    table <- get
    case phase table of

        DealHands -> dealHandsStep 

        PreFlop -> preFlopStep
        -- First betting rounds, player can bet, fold, call, raise. (logic we don't have yet)
        Flop -> dealCommunityCards

        Turn -> dealCommunityCards

        River -> dealCommunityCards

        Showdown -> table -- Here we have more betting logic, showing hands to all players?





-- | Initialisation of the blinds of the game.
initiateBlinds :: State Table ()
intiateBlinds = do
    table <- get
    -- We need to know where the SB and BB is.
    let sbPlayer = (smallBlindPosition table)
        bbPlayer = (bigBlindPosition table)
        playersList = (players table)

        sb = 50
        bb = 100 -- We can later on introduce logic that increases blind amount per round

    placeBet (playersList !! sbPlayer) sb
    placeBet (playersList !! bbPlayer) bb

    





startOfGame :: Table -> Bool
startOfGame table | (table phase) == DealHands = True
                  | (table phase) == _         = False


resetGameState :: State Table Table


-- | Move where the dealer,SB, and BB are on the table. Mod helps us wrap around.
moveDealer :: State Table ()
moveDealer = do
    table <- get
    let numPlayers = length (players table)
        newDealerPosition     = ((dealerPosition table) + 1) `mod` numPlayers
        newSmallBlindPosition = ((newDealerPosition table) + 1) `mod` numPlayers
        newBigBlindPosition   = ((newSmallBlindPosition table) + 1) `mod` numPlayers

    modify (\table -> table 
        { dealerPosition     = newDealerPosition,
          smallBlindPosition = newSmallBlindPosition,
          bigBlindPosition   = newBigBlindPosition
        })




--------------------------------------------------------------
---------- Dealing of cards, hands, communitycards -----------

-- | Deal cards, update the deck after removing the cards
--   and return the cards that have been delt.
dealCards :: Int -> State Table [Card]
dealCards n = do
    table <- get
    let (deltCards, newDeck) = splitAt n (deck table)
    put table { deck = newDeck }
    return deltCards

-- | Deal hands to the players. Get the table and list of players, take the list of players
--   and for each player in the list, use 'dealCards' to take two cards from the deck. Put
--   the cards as the players hand, then put the list of players now with updated hands back
--   into the table.
dealHands :: State Table ()
dealHands2 = do
    table <- get
    playerList <- gets players
    playerList' <- mapM (\player -> do
        deltHand <- dealCards 2
        put player { hand = deltHand}
        ) playerList
    put table { players = playerList' }

-- | Apply the functions to dealHands to players along with moving to the next game phase.
dealHandsStep :: State Table ()
dealHandsStep = do
    dealHands
    moveToNextPhase


preFlopStep :: State Table ()
preFlopStep = do
    table <- get
    if bettingFinished table
        then moveToNextPhase
        else bettingRound
        -- Also want fold and check: player Actions.



-- | Deal community cards to the board.
dealCommunityCards :: State Table ()
flopStep = do
    table <- get
    let cCards = do
        case phase table of
            Flop -> dealCards 3
            River -> dealCards 1
            Turn -> dealCards 1
            _ -> return []
    modify (\table -> table { board = board table ++ cCards })
    moveToNextPhase    





--onlyOnePlayerLeft :: State Table Bool
--onlyOnePlayerLeft table = 







                                
-- | Advance from the tables current phase to the next one.
moveToNextPhase :: State Table ()
moveToNextPhase = modify (\table -> table { phase = nextPhase (phase table) })



moveButton :: State Table Table
movebutton = do
    table <- get
    let playersAtTable = length (players table)



{-
winner :: State Table Player
    winner = do
        table <- get
        playersList <- gets players
        nonFoldedPlayers = mapM (\players -> )
-}


-- Temporary gameloop as a starting point
gameLoop :: Table -> IO ()
gameLoop table = do
    print table
    let newTable = gameStep table
    let newTable' = gameStep newTable
    let newTable'' = gameStep newTable'
    print newTable''
 

