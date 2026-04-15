module Types where


---------------------------------------------------------------------
-----------------------
 -- | All different suits
data Suit = Hearts | Spades | Diamonds | Clubs 
-----------------------
    deriving (Show, Eq, Ord)
-- | All different ranks
data Rank =  Num Int | Jack | Queen | King | Ace 
    deriving (Show, Eq, Ord)

-----------------------
 -- | Card has an rank and a suit
data Card = Card Rank Suit deriving 
    (Eq)
                  
 -- | to show the cards in a nice way
instance Show Card where
  show (Card r s) = formatedRank r ++ formatedSuit s

-- | To compere two cards
instance Ord Card where
  compare (Card r1 s1) (Card r2 s2) = 
      if r1 == r2 then compare s1 s2 else compare r1 r2

formatedRank :: Rank -> String
formatedRank (Num n) = show n
formatedRank rank    = case rank of
                        Jack -> "J"
                        Queen -> "Q"
                        King -> "K"
                        Ace -> "A"

formatedSuit :: Suit -> String
formatedSuit s 
            | s == Spades = "\9824"
            | s == Hearts = "\9829"
            | s == Diamonds = "\9830"
            | s == Clubs = "\9827"

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
    deriving (Show)

---------------------------------------------------------------------
-- | All phases of the game
data GamePhase = 
            Dealhands
            | PreFlop
            | Flop
            | Turn
            | River
            | Showdown
    deriving (Show)

---------------------------------------------------------------------
-- | Data type for a player
data Player = Player
            {
            name          :: String,
            hand          :: Hand,
            chips         :: Chip,
            commitedChips :: Chip,
            folded        :: Bool,
            blind         :: Blind
            }
    deriving (Show)

---------------------------------------------------------------------
-- | The table represent the gamestate
data Table = Table
            {
            players      :: [Player],
            highBet      :: Bet,
            deck         :: Deck,
            board        :: CommunityCard,
            phase        :: GamePhase,
            pot          :: Pot
            --dealPosition :: Int
            }
    deriving (Show)