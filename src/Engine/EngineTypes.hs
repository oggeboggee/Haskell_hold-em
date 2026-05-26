--{-# LANGUAGE DeriveGeneric #-}

module Engine.EngineTypes where

import Control.Monad.State
import Test.QuickCheck


--------------------------------------------------------------------------------------------------------
-- EVENTS (input to the engine)
--------------------------------------------------------------------------------------------------------

-- | An Event sent into the game engine. Represents an intention to change the game state.
-- | PlayerEvent: a player does something (fold, call, raise, etc.)
-- | EngineEvent: the engine does something automatically (deal blinds, run showdown, etc.)
data Event
    = PlayerEvent PlayerIndex Action
    | EngineEvent EngineAction 
    deriving(Show)

-- | Internal actions initiated automatically by the game engine.
data EngineAction 
    = PlaceBlind PlayerIndex BlindType Bet      -- Place a blind. Parameters needed: which player, what blindtype and how much.
    | RunShowdown                               -- Evaluate hands and dsitribute pot. No parameters needed.
    | EliminatePlayers                          -- Remove players with zero chips after a showdown. 
    deriving (Show)


--------------------------------------------------------------------------------------------------------
-- GAME EVENTS (output from the engine)
--------------------------------------------------------------------------------------------------------

-- | Produced by the engine after processing an Event
-- | Describes what actually happened. USed for broadcasting to clients.
data GameEvent 
    -- Player events
    = PlayerFolded PlayerName
    | PlayerChecked PlayerName
    | PlayerCalled PlayerName Bet
    | PlayerRaised PlayerName Bet
    | PlayerAllIn PlayerName Bet

    -- Engine events
    | PlayerPlacedBlinds PlayerName BlindType Bet  -- Result of PlaceBlind
    | ShowdownHappened [PlayerName]                -- Result of RunShowdown
    | HandStarted [PlayerName]                     -- Result of startHand setup
    | PhaseChanged GamePhase                       -- Result of advancePhase
    | PlayerEliminated [PlayerName]                -- Result of EliminatePlayers
    | ChipsAwarded [(PlayerName, Chip)]            -- Result of RunShowdown
    deriving (Show)


data GameStepResult
    = AwaitingAction 
    --Bettinground in progress, wait.
    | PhaseAdvanced [GameEvent] Table 
    -- Betting round finished. Engine delt community cards, reset etc.
    | HandComplete [GameEvent] Table
    -- Hand over, events have Showdownhappened and winning player(s)
    -- Table has dsitributed chips, cleared pot.


--------------------------------------------------------------------------------------------------------
-- PLAYER ACTIONS
--------------------------------------------------------------------------------------------------------

data Action
    = Check
    | Fold
    | Call
    | Raise Int
    | AllIn
    deriving (Eq)

instance Show Action where
    show Check     = "Check"
    show Fold      = "Fold"
    show Call      = "Call"
    show (Raise x) = "Raise " ++ show x
    show AllIn     = "All-in"


--------------------------------------------------------------------------------------------------------
-- CARDS
--------------------------------------------------------------------------------------------------------

data Suit = Hearts | Spades | Diamonds | Clubs
    deriving (Eq, Ord, Enum, Bounded)

instance Show Suit where
    show s = case s of
        Spades -> "\9824" --Spades -> "S"
        Hearts -> "\9829" --Hearts -> "H"
        Diamonds -> "\9830" --Diamonds -> "D"
        Clubs -> "\9827" --Clubs -> "C"

instance Arbitrary Suit where
  arbitrary = elements [minBound .. maxBound]


data Rank 
    = Two | Three | Four | Five | Six | Seven | Eight
    | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Bounded)

instance Show Rank where
  show r = case r of
    Two   -> "2"
    Three -> "3"
    Four  -> "4"
    Five  -> "5"
    Six   -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine  -> "9"
    Ten   -> "10"
    Jack  -> "J"
    Queen -> "Q"
    King  -> "K"
    Ace   -> "A"

instance Arbitrary Rank where
  arbitrary = elements [minBound .. maxBound]


data Card = Card Rank Suit 
    deriving (Eq)

instance Show Card where
  show (Card r s) = show r ++ show s

-- | To compere two cards
instance Ord Card where
  compare (Card r1 s1) (Card r2 s2) =
      if r1 == r2 then compare s1 s2 else compare r1 r2

instance Arbitrary Card where
    arbitrary = do 
        rank <- arbitrary
        Card rank <$> arbitrary

--------------------------------------------------------------------------------------------------------
-- TYPE ALIASES
--------------------------------------------------------------------------------------------------------

type PlayerName = String        -- A players name
type PlayerIndex = Int          -- An internal reference to a player
type Chip = Int                 -- Chips
type Bet = Chip                 -- A bet of chips
type Pot = Int                  -- The pot of the table
type Hand = [Card]              -- The two cards a player has in their hand
type CommunityCard = [Card]     -- The common cards on the table (board)
type Deck = [Card]              -- The full deck of cards.


--------------------------------------------------------------------------------------------------------
-- BLINDS AND PHASES
--------------------------------------------------------------------------------------------------------

data BlindType =
    SmallBlind
    | BigBlind
    deriving (Show, Eq)

-- | All phases of the game
data GamePhase 
    = DealHands
    | PreFlop
    | Flop
    | Turn
    | River
    | Showdown
    deriving (Show, Eq)


--------------------------------------------------------------------------------------------------------
-- PLAYER
--------------------------------------------------------------------------------------------------------

data Player = Player
    { name          :: String
    , hand          :: Hand
    , chips         :: Chip
    , commitedChips :: Chip
    , folded        :: Bool
    , acted         :: Bool
    }
    deriving (Eq)

instance Show Player where
    show p = 
        "Name: " ++ show (name p) ++
        --" | Hand: " ++ show (hand p) ++
        " | Chips: " ++ show (chips p) ++
        " | Pot contribution: " ++ show (commitedChips p) ++
        " | Folded: " ++ show (folded p) ++
        " | Acted: " ++ show (acted p)


--------------------------------------------------------------------------------------------------------
-- TABLE (full game state)
--------------------------------------------------------------------------------------------------------

data Table = Table
    { players            :: [Player]
    , highBet            :: Bet
    , bets               :: [Bet]
    , deck               :: Deck
    , board              :: CommunityCard
    , phase              :: GamePhase
    , pot                :: Pot
    , dealerPosition     :: Int
    , smallBlindPosition :: Int
    , bigBlindPosition   :: Int
    }

instance Show Table where
    show t = 
        "Players:\n"        ++ unlines (map show (players t)) ++
        "Phase: "           ++ show (phase t) ++ "\n" ++
        "CommmunityCards: " ++ show (board t) ++ "\n" ++
        "Highbet:         " ++ show (highBet t) ++ "\n" ++
        "Pot: "             ++ show (pot t) ++ "\n" ++
        "Dealer: "          ++ name (players t !! dealerPosition t)
        --"sb: " ++ name (players t!!smallBlindPosition t) ++
        --"bb: " ++ name (players t!!bigBlindPosition t)


--------------------------------------------------------------------------------------------------------
-- OTHERS
--------------------------------------------------------------------------------------------------------

-- | StateT lets us thread Table through IO actions
type Game = StateT Table IO

 -- | Hand combinations
data Combination 
    = HighCard | Pair | TwoPairs | ThreeOfAKind | Straight
    | Flush | FullHouse | Quads | StraightFlush
    deriving (Show, Eq, Ord)

-- | Not used currently
data PlayerState =
    NotActed
    | HasFolded
    | HasChecked
    | HasBet
    | HasAllin
    deriving (Eq)
