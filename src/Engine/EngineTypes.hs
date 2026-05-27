module Engine.EngineTypes where

--import Control.Monad.State
import Test.QuickCheck

--------------------------------------------------------------------------------------------------------
-- EVENTS (input to the engine)
--------------------------------------------------------------------------------------------------------

-- | Represents an intention to change the game state
--   PlayerEvent: wraps an action taken by a player player (fold, call, raise, etc.)
--   EngineEvent: wraps an action taken by the engine (place blinds, run showdown, etc.)
data Event
    = PlayerEvent PlayerIndex Action
    | EngineEvent EngineAction 
    deriving(Show)

-- | Internal actions initiated automatically by the game engine.
--   Currently only used for placing blinds at the start of a hand (the others aren't being used applied explicitly right now).
data EngineAction 
    = PlaceBlind PlayerIndex BlindType Bet      -- Place a blind. Parameters needed: which player, what blindtype and how much.
    | RunShowdown                               -- Evaluate hands and dsitribute pot. No parameters needed.
    | EliminatePlayers                          -- Remove players with zero chips after a showdown. 
    deriving (Show)


--------------------------------------------------------------------------------------------------------
-- GAME EVENTS (output from the engine)
--------------------------------------------------------------------------------------------------------

-- | Produced by the engine after processing an Event
--   Describes what actually happened. Used for broadcasting to clients.
data GameEvent 
    -- Player-initiated events
    = PlayerFolded PlayerName
    | PlayerChecked PlayerName
    | PlayerCalled PlayerName Bet
    | PlayerRaised PlayerName Bet
    | PlayerAllIn PlayerName Bet

    -- Engine-initiated  events
    | PlayerPlacedBlinds PlayerName BlindType Bet  -- Result of PlaceBlind
    | ShowdownHappened [PlayerName]                -- Result of RunShowdown
    | HandStarted [PlayerName]                     -- Result of startHand setup
    | PhaseChanged GamePhase                       -- Result of advancePhase
    | PlayerEliminated [PlayerName]                -- Result of EliminatePlayers
    | ChipsAwarded [(PlayerName, Chip)]            -- Result of RunShowdown
    deriving (Show, Eq)

-- | Returned by 'stepGame' after every player action to tell the server what happened and
--   wether it needs to do anything before waiting for the next action.
data GameStepResult
    -- Betting round is still live, server should wait for the next player action.
    = AwaitingAction 

    -- A betting round ended and the engine advanced to the next phase, delt card, and resetting blind state. Server
    -- should broadcast and call stepGame in case the new phase also needs advancing to the next (e.g all players are all in).
    | PhaseAdvanced [GameEvent] Table 

    -- The hand is over, the engine ran the showdown, awarded chips and eliminated players. Server should 
    -- broadcast the results and check if a new hand can start.
    | HandComplete [GameEvent] Table



--------------------------------------------------------------------------------------------------------
-- PLAYER ACTIONS
--------------------------------------------------------------------------------------------------------

-- | Actions a player can take during their turn
data Action
    = Check
    | Fold
    | Call
    | Raise Int     -- Raise by this amount on top of calling the bet.
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

-- | A standard playing card with a 'Rank' and a 'Suit'
data Card = Card Rank Suit 
    deriving (Eq)

instance Show Card where
  show (Card r s) = show r ++ show s

-- | To compere two cards, cards are ordered by rank first and then suit.
instance Ord Card where
  compare (Card r1 s1) (Card r2 s2) =
      if r1 == r2 then compare s1 s2 else compare r1 r2

instance Arbitrary Card where
    arbitrary = do 
        rank <- arbitrary
        Card rank <$> arbitrary

-- | The suits a playing card can have
data Suit = Hearts | Spades | Diamonds | Clubs
    deriving (Eq, Ord, Enum, Bounded)

-- | Display cards in a nice way.
instance Show Suit where
    show s = case s of
        Spades -> "\9824" --Spades -> "S"
        Hearts -> "\9829" --Hearts -> "H"
        Diamonds -> "\9830" --Diamonds -> "D"
        Clubs -> "\9827" --Clubs -> "C"

instance Arbitrary Suit where
  arbitrary = elements [minBound .. maxBound]


-- | The ranks a playing card can have
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


--------------------------------------------------------------------------------------------------------
-- TYPE ALIASES
--------------------------------------------------------------------------------------------------------

type PlayerName    = String       -- A players chosen display name
type PlayerIndex   = Int          -- An internal reference to a player, their position in the table's players list.
type Chip          = Int          -- Chips
type Bet           = Chip         -- A bet of chips
type Pot           = Int          -- The pot of the table
type Hand          = [Card]       -- The two cards a player has in their hand
type CommunityCard = [Card]       -- The common cards on the table (board)
type Deck          = [Card]       -- The full ordered deck of cards.


--------------------------------------------------------------------------------------------------------
-- BLINDS AND PHASES
--------------------------------------------------------------------------------------------------------

-- | The place blinds can be a small blind or a big blind.
data BlindType =
    SmallBlind
    | BigBlind
    deriving (Show, Eq)

-- | The phases of a Haskell Hold'em hand in correct order.
--   The engine moves through these sequentially for each hand.
data GamePhase 
    = DealHands     -- The initial state, hands are delt and blinds are placed here.
    | PreFlop       -- The first betting round, board is still empty.
    | Flop          -- The second betting round, three cards are place on the board.
    | Turn          -- The third betting round, the fourth card is placed on the board.
    | River         -- The last betting round, the final card is placed on the board.
    | Showdown      -- The hands are evaluated, winner(s) selected and the pot awarded.
    deriving (Show, Eq)


--------------------------------------------------------------------------------------------------------
-- PLAYER
--------------------------------------------------------------------------------------------------------

-- | Represents a player seated at the table and their current state for the hand.
data Player = Player
    { name          :: PlayerName   -- The players display name.
    , hand          :: Hand         -- The players two cards in hand
    , chips         :: Chip         -- The players current chip count.
    , commitedChips :: Chip         -- How many chips the player has put in the pot.
    , folded        :: Bool         -- Has the player folded? True | False
    , acted         :: Bool         -- Has the player acted this betting round? True | False
    }
    deriving (Eq)

instance Show Player where
    show p = 
        "Name: "                ++ show (name p) ++
        " | Hand: "             ++ show (hand p) ++
        " | Chips: "            ++ show (chips p) ++
        " | Pot contribution: " ++ show (commitedChips p) ++
        " | Folded: "           ++ show (folded p) ++
        " | Acted: "            ++ show (acted p)


--------------------------------------------------------------------------------------------------------
-- TABLE (full game state)
--------------------------------------------------------------------------------------------------------

-- | The complete game state. This gets threaded through the engine via State Table. Contains
--   all players, the deck, board, pot, and positional information etc.
data Table = Table
    { players            :: [Player]        -- The players seated at the table.
    , highBet            :: Bet             -- The current highest bet this round that other players must match.
    , bets               :: [Bet]           -- History of all placed bets this round.
    , deck               :: Deck            -- The deck of cards used this hand.
    , board              :: CommunityCard   -- The shared cards currently on the board.
    , phase              :: GamePhase       -- The current phase of the hand.
    , pot                :: Pot             -- The total chips placed in the pot this hand.
    , dealerPosition     :: Int             -- Index of the player holding the dealer button.
    , smallBlindPosition :: Int             -- Index of the player in the small blind position.
    , bigBlindPosition   :: Int             -- Index of the player in the big blind position.
    }

instance Show Table where
    show t = 
        "Players:\n"        ++ unlines (map show (players t)) ++
        "Phase: "           ++ show (phase t)                 ++ "\n" ++
        "CommmunityCards: " ++ show (board t)                 ++ "\n" ++
        "Highbet:         " ++ show (highBet t)               ++ "\n" ++
        "Pot: "             ++ show (pot t)                   ++ "\n" ++
        "Dealer: "          ++ name (players t !! dealerPosition t)


--------------------------------------------------------------------------------------------------------
-- HAND EVALUATION
--------------------------------------------------------------------------------------------------------

 -- | All the possible hand combinations in ascending order of strength. Used by
 --   handEvaluation to rank and compare player hands at showdown in order to select winner(s).
data Combination 
    = HighCard | Pair | TwoPairs | ThreeOfAKind | Straight
    | Flush | FullHouse | Quads | StraightFlush
    deriving (Show, Eq, Ord)