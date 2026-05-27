module Engine.TexasEngine 
    ( runShowdown
    , startHand
    , stepGame
    ) where

import Engine.EngineTypes
import Engine.Cards
import Engine.Actions
import Engine.Utilities

-- Packages
import Control.Monad.State
import System.Random
import Data.Either

--------------------------------------------------------------------------------------------------------
-- HAND SETUP
--------------------------------------------------------------------------------------------------------
-- | Initialises a new hand. Called by the server with externally generated randomness.
--
-- Sequence:
--   1. moveDealer      — rotate the dealer button and recalculate SB/BB positions.
--   2. resetTable      — clear board, pot, bets, chips committed, and reset phase to DealHands.
--   3. runShuffle      — shuffle the deck using the provided random generator.
--   4. dealHands       — deal two hole cards to each seated player.
--   5. initiateBlinds  — post small and big blinds, returning those as GameEvents.
--   6. moveToNextPhase — advance from DealHands to PreFlop, ready for betting.
startHand :: StdGen -> Table -> ([GameEvent], Table) -- startHand :: StdGen -> State Table [GameEvent]
startHand gen = runState setupHand                   -- startHand gen = setupHand
    where
        setupHand :: State Table [GameEvent]
        setupHand = do
            moveDealer
            resetTable
            runShuffle gen
            dealHands
            placeBlinds <- initiateBlinds
            moveToNextPhase
            pure placeBlinds

--------------------------------------------------------------------------------------------------------
-- GAME PROGRESSION
--------------------------------------------------------------------------------------------------------
-- | Called by the server after every successful player action to determine what happens next.
--   Returns one of three outcomes:
--
--   AwaitingAction — betting round is still live; server waits for the next player action.
--   PhaseAdvanced  — betting round ended; engine has advanced the phase, dealt community cards,
--                     and reset betting state. Server should broadcast and call stepGame again
--                     in case the new phase also needs auto-advancing (e.g. all-in runout).
--   HandComplete   — hand is over, engine has run the showdown and awarded chips.
stepGame :: Table -> GameStepResult
stepGame t
    -- Only one active player remains — everyone else folded. Skip showdown, award pot directly.
    | handOver t =
        let (events, t') = runState runShowdown t
        in HandComplete events t'

    -- Betting round is over, check phsae transition and depending on the phase decide showdown or advance
    | bettingRoundOver t = 
        case phase t of
            -- River betting is done, run the showdown to evaluate hands and award the pot.
            River ->
                let (events, t') = runState runShowdown t
                in HandComplete events t'
            -- Any other phase: advance to the next phase and deal the appropriate community cards.
            _ -> 
                let (_, t') = runState advancePhase t
                in PhaseAdvanced [] t'
    
    -- Betting round is still active, a player still needs to act.
    | otherwise = AwaitingAction



-- | Helper function used by stepGame when a betting round is finished.
--   Advances the phase, deals communitycards and resets the betting state.
--   This logic previously lived in runPhase.
advancePhase :: State Table ()
advancePhase = do
    moveToNextPhase     -- Increment the phase: e.g PreFlop -> Flop
    dealCommunityCards  -- Deal 3 cards on Flop, 1 on Turn and 1 on River.
    resetBettingRound   -- Clear 'acted' flags and commitedChips.
                

--------------------------------------------------------------------------------------------------------
-- TABLE RESETTING
--------------------------------------------------------------------------------------------------------
-- | Reset the required fields in the table and the players in the table.
--   Called at the start of each hand before cards are delt.
resetTable :: State Table () 
resetTable =
    modify (\t ->
        let resetPlayer p = p { folded = False, acted = False, commitedChips = 0 }
        in t
            { players = map resetPlayer (players t)
            , highBet = 0
            , pot     = 0
            , bets    = []
            , board   = []
            , phase   = DealHands
            }
    )


-- | Reset after each betting round by clearing flags for players. Also resets relevant fields in the table.
--   Called at the end of every betting round before next phase.
--   Special case: during PreFlop the highBet is preserved (100, the big blind amount)
--   so that players still need to call or raise relative to the BB, not zero.
--   On all other phases the highBet resets to zero so postflop betting starts fresh.
resetBettingRound :: State Table () 
resetBettingRound = do
    table <- get
    let hb = case phase table of
                PreFlop -> highBet table    -- Keep BB as the high bet for preflop.
                _       -> 0

    modify (\t -> t { players = map (\p -> p { acted = False, commitedChips = 0 }) (players t), highBet = hb})


--------------------------------------------------------------------------------------------------------
-- BLIND INITIATION
--------------------------------------------------------------------------------------------------------
-- | Place the small and big blinds at the start of a hand.
--   Uses applyEvent internally so that the blinds also go through the same chip deduction
--   and pot incrementing logic as normal player actions.
--   Any left errors are silently discarded as it should never fail on a reset table.
--   Returns the resulting GameEvents (PlayerPlacedBlinds) for broadcasting.
initiateBlinds :: State Table [GameEvent]
initiateBlinds = do

    sbPlayerIdx <- gets smallBlindPosition
    bbPlayerIdx <- gets bigBlindPosition

    sbResult <- applyEvent (EngineEvent (PlaceBlind sbPlayerIdx SmallBlind 50))
    bbResult <- applyEvent (EngineEvent (PlaceBlind bbPlayerIdx BigBlind 100))

    pure (concat (rights [sbResult, bbResult]))


--------------------------------------------------------------------------------------------------------
-- ROTATE DEALER
--------------------------------------------------------------------------------------------------------
-- | Move where the dealer, SB, and BB are on the table. Mod helps us wrap around. the players list.
--   Rotates the dealer button around the table and updates SB and BB positions based on where the dealer
--   button ends up. 
--   For 2 players: dealer = SB and the other player = BB.
--   For 3+ players: SB is left of dealer and BB is two left of dealer.
moveDealer :: State Table ()
moveDealer = do
    table <- get
    let numPlayers = length (players table)
        newDealerPosition = (dealerPosition table + 1) `mod` numPlayers

    if numPlayers == 2
        then modify (\t -> t
            { dealerPosition     = newDealerPosition
            , smallBlindPosition = newDealerPosition
            , bigBlindPosition   = (newDealerPosition + 1) `mod` numPlayers
            })
        else modify (\t -> t
            { dealerPosition     = newDealerPosition
            , smallBlindPosition = (newDealerPosition + 1) `mod` numPlayers
            , bigBlindPosition   = (newDealerPosition + 2) `mod` numPlayers
            })
  

--------------------------------------------------------------------------------------------------------
-- SHUFFLING
--------------------------------------------------------------------------------------------------------

-- | Return a shuffled deck with 52 cards using the provided random generator and store it on the table.
runShuffle :: StdGen -> State Table ()
runShuffle gen = do
    let (doubles, _) = randomDoubles' 52 gen
    let shuffledDeck = shuffle doubles fullDeck
    modify (\t -> t { deck = shuffledDeck })

-- | Generates a list of n random Doubles [0,1] from the StdGen
--   USed by 'shuffle' in Cards.hs
randomDoubles' :: Int -> StdGen -> ([Double], StdGen)
randomDoubles' 0 gen = ([], gen)
randomDoubles' n gen = (x:xs, gen2)
  where
    (x, gen1)  = randomR (0.0,1.0) gen
    (xs, gen2) = randomDoubles' (n-1) gen1


--------------------------------------------------------------------------------------------------------
-- Dealing
--------------------------------------------------------------------------------------------------------

-- | Deal cards, update the deck after removing the cards and return the cards that have been delt.
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
dealHands = do
    playerList <- gets players
    playerList' <- mapM (\player -> do
        deltHand <- dealCards 2
        return player { hand = deltHand}
        ) playerList
    modify (\t -> t { players = playerList' })


-- | Deal community cards to the board based on the current phase.
--   Flop: 3 cards, Turn: 1 card, River: 1 card.
dealCommunityCards :: State Table ()
dealCommunityCards = do
    currentPhase <- gets phase
    cards <- case currentPhase of
        Flop  -> dealCards 3
        Turn  -> dealCards 1
        River -> dealCards 1
        _     -> return []
    modify (\t -> t { board = board t ++ cards })

--------------------------------------------------------------------------------------------------------
-- PHASE
--------------------------------------------------------------------------------------------------------

-- | Advances the tables phase to the nxt one in the sequence.
moveToNextPhase :: State Table ()
moveToNextPhase = modify (\t -> t { phase = nextPhase (phase t) })
