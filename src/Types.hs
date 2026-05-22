module Types where
import Control.Monad.State
import Test.QuickCheck

-- | https://wiki.haskell.org/Real_World_Applications/Event_Driven_Applications

-- | Attempt at making event data types.
-- | Should we have events for players that are actions and then also have Events for things such as
-- | dealing cards/hands, resetting rounds, etc? Does Blinds belong in this category. Thinking in terms
-- | of future if we will need to send this in JSON? https://academy.fpblock.com/haskell/library/aeson/

-- | INPUT EVENTS
-- | An Event sent into the gameengine. An intention to change the game state.
data Event = 
    PlayerEvent PlayerIndex Action    -- Player attempts to perform an action.
    | EngineEvent EngineAction        -- Game engine trigers something.

-- | ENGINE ACTIONS
-- | Internal actions initiated automatically by the game.
data EngineAction =
    PlaceBlind PlayerIndex BlindType Bet   -- Engine places blind for player
    -- | AdvancePhase
    | RunShowdown                          -- Engine initiates showdown.

-- | OUTPUT EVENTS
-- | Specific events produced by the game after an Event is processed. Descirbe what happened and
-- | is used for output or to log what happened.
data GameEvent =
    PlayerFolded PlayerName
    | PlayerChecked PlayerName
    | PlayerCalled PlayerName Bet
    | PlayerRaised PlayerName Bet
    | PlayerAllIn PlayerName Bet
    | PlayerPlacedBlinds PlayerName BlindType Bet
    | ShowdownHappened [PlayerName]
    deriving (Show, Eq)

-- | A players name (used for output)
type PlayerName = String
-- | An internal reference to a player. 
type PlayerIndex = Int

-- Data type for all the different types of actions a player can make
data Action = 
    Check
    | Fold
    | Call
    | Raise Int
    | AllIn
    deriving (Eq)

instance Show Action where
    show Check = "Check"
    show Fold = "Fold"
    show Call = "Call"
    show (Raise x) = "Raise " ++ show x
    show AllIn = "All-in"


--------------------------------------------
-- | https://cstml.github.io/2021/07/22/State-Monad.html
-- | We can use StateT to get access to IO inside the state monad.
-- I'll declare a new data type to see how this works.
-- We can use liftIO for now to get a working game in the terminal.
type Game = StateT Table IO

---------------------------------------------------------------------
-----------------------
 -- | All different suits
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

-- | All different ranks
data Rank =  Two 
            | Three 
            | Four 
            | Five 
            | Six 
            | Seven 
            | Eight 
            | Nine 
            | Ten 
            | Jack 
            | Queen 
            | King 
            | Ace 
  deriving (Eq, Ord, Enum, Bounded)

instance Show Rank where
  show r = case r of
    Two -> "2"
    Three -> "3"
    Four -> "4"
    Five -> "5"
    Six -> "6"
    Seven -> "7"
    Eight -> "8"
    Nine -> "9"
    Ten -> "10"
    Jack  -> "J"
    Queen -> "Q"
    King  -> "K"
    Ace   -> "A"

instance Arbitrary Rank where
  arbitrary = elements [minBound .. maxBound]

---------------------------------------------------------------------
 -- | Card has an rank and a suit
data Card = Card Rank Suit deriving 
    (Eq)


 -- | to show the cards in a nice way
instance Show Card where
  show (Card r s) = show r ++ show s

-- | To compere two cards
instance Ord Card where
  compare (Card r1 s1) (Card r2 s2) = 
      if r1 == r2 then compare s1 s2 else compare r1 r2

instance Arbitrary Card where

  arbitrary = do
    rank <- arbitrary
    suit <- arbitrary
    return (Card rank suit)


---------------------------------------------------------------------
-- | Hand is the two cards a player have on hand
type Hand = [Card]
-- | CommunityCards are the common cards on the table
type CommunityCard = [Card]
-- | Deck is a list of cards
type Deck = [Card]
---------------------------------------------------------------------
 -- | Combinations
data Combination =  HighCard
                  | Pair 
                  | TwoPairs 
                  | ThreeOfAKind 
                  | Straight 
                  | Flush 
                  | FullHouse 
                  | Quads 
                  | StraightFlush
                  deriving (Show, Eq, Ord)

---------------------------------------------------------------------
-- | Chips and the pot is represented as Int
type Chip = Int
type Bet = Chip
type Pot = Int

---------------------------------------------------------------------
-- | Represent if a player have big, samll or no blind
data BlindType = 
    SmallBlind
    | BigBlind
    deriving (Show, Eq)

---------------------------------------------------------------------
-- | All phases of the game
data GamePhase = 
            DealHands
            | PreFlop
            | Flop
            | Turn
            | River
            | Showdown
    deriving (Show, Eq)


---------------------------------------------------------------------
-- | Data type for a player

data Player = Player
            {
            name          :: String,
            hand          :: Hand,
            chips         :: Chip,
            commitedChips :: Chip,
            folded        :: Bool,
            acted         :: Bool
            }
    deriving (Eq)

instance Show Player where
    show p = "Name: " ++ show (name p) ++
           " Hand: " ++ show (hand p) ++ 
           " Chips: " ++ show (chips p) ++
           " Pot contribution: " ++ show (commitedChips p) ++
           " Folded: " ++ show (folded p) ++
           " Acted: " ++ show (acted p)
           --" Blind:" ++ show (blind p) ++ "\n"
---------------------------------------------------------------------
-- | The table represent the gamestate
data Table = Table
            {
            players       :: [Player],
            highBet       :: Bet,
            bets          :: [Bet],
            deck          :: Deck,
            board         :: CommunityCard,
            phase         :: GamePhase,
            pot           :: Pot,
            dealerPosition :: Int,
            smallBlindPosition :: Int,
            bigBlindPosition :: Int
            }

instance Show Table where
    show t = "Players:\n" ++ unlines (map show (players t)) ++ 
            --"\nTable State(print): " ++ 
            " \nPhase:" ++ show (phase t) ++
            " \nCommmunityCards: " ++ show (board t) ++
            " \nPot:         " ++ show (pot t) ++
            " \nHighbet:         " ++ show (highBet t) ++
            " \nsb: " ++ name (players t!!smallBlindPosition t) ++ 
            "\nbb: " ++ name (players t!!bigBlindPosition t)


---------------------------------------------------------------------



data PlayerState =
    NotActed
    | HasFolded
    | HasChecked
    | HasBet
    | HasAllin
    deriving (Eq)
