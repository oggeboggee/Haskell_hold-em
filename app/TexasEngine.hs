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

gameRound :: Game ()
gameRound = do
    liftIO $ putStrLn "Game is starting.."
    resetGameState


    liftIO $ putStrLn "Initiating blinds.."
    initiateBlinds

    liftIO $ putStrLn "Dealing hands.."
    dealHands

    table <- get
    liftIO $ print table

    --bettingRound
{-
    moveToNextPhase
    dealCommunityCards
    bettingRound

    moveToNextPhase
    dealCommunityCards
    bettingRound

    moveToNextPhase
    dealCommunityCards
    bettingRound

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
                  checked = False
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
initiateBlinds :: Game ()
initiateBlinds = do
    table <- get

    let sbPlayer = (smallBlindPosition table)
        bbPlayer = (bigBlindPosition table)

    placeBet sbPlayer 50
    placeBet bbPlayer 100
 -- We can later on introduce logic that increases blind amount per round



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

