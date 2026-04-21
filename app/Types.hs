module Types where
import Control.Monad.State

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
    deriving (Eq, Ord)

instance Show Suit where
    show s = case s of
        Spades -> "S"
        Hearts -> "H"
        Diamonds -> "D"
        Clubs -> "C"

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
  deriving (Eq, Ord)

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
data Blind = 
    NoBlind
    | SmallBlind
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
    deriving (Show)

---------------------------------------------------------------------
-- | Table positions a player can be in.
data TablePosition =
    Dealer
    | SB
    | BB
    | UTG
    | CutOff
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
            checked       :: Bool,
            blind         :: Blind
            --position      :: TablePosition
            }
    deriving (Eq)

instance Show Player where
    show p = "Name:" ++ show (name p) ++
           " Hand:" ++ show (hand p) ++ 
           " Chips:" ++ show (chips p) ++
           " Commited:" ++ show (commitedChips p) ++
           " Folded:" ++ show (folded p) ++
           " Blind:" ++ show (blind p) ++ "\n"
---------------------------------------------------------------------
-- | The table represent the gamestate
data Table = Table
            {
            players       :: [Player],
            --playerTurn    :: (Player, Int), -- We have this with dealerposition.
            activePlayers :: [Player],
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
    -- deriving (Show)
instance Show Table where
    show t = show (players t) ++ 
    --        " Playerturn:" ++ show (name (fst (playerTurn t))) ++
            " \nHighbet:" ++ show (highBet t) ++
            " \nPot:" ++ show (pot t)

