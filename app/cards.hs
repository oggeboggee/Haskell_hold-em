{- Card related data-types -}

module Cards where


 -- | All different suits
data Suit = Hearts | Spades | Diamonds | Clubs deriving (Eq, Ord)

-- | All different ranks

data Rank =   Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace 
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

rankValue :: Rank -> Int
rankValue r = case r of
  Two   -> 2
  Three -> 3
  Four  -> 4
  Five  -> 5
  Six   -> 6
  Seven -> 7
  Eight -> 8
  Nine  -> 9
  Ten   -> 10
  Jack  -> 11
  Queen -> 12
  King  -> 13
  Ace   -> 14


instance Show Suit where
  show s = case s of
    Spades -> "♠"
    Hearts -> "♥"
    Diamonds -> "♦"
    Clubs -> "♣"


 -- | Card has an rank and a suit
data Card = Card Rank Suit deriving (Eq, Ord)


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

                  

 -- | to show the cards in a nice way
instance Show Card where
  show (Card r s) = show r ++ show s
{-
formatedRank :: Rank -> String
formatedRank (Num n) = show n
formatedRank rank    = show rank


formatedSuit :: Suit -> String
formatedSuit s 
            | s == Spades = "\9824"
            | s == Hearts = "\9829"
            | s == Diamonds = "\9830"
            | s == Clubs = "\9827"
-}
 -- | Hand is a list of cards
type Hand = [Card]

-- | Deck is a list of cards
type Deck = [Card]

-- | Extract the rank of a card
rank :: Card -> Rank
rank (Card r _) = r

-- | Extract the suit of a card
suit :: Card -> Suit
suit (Card _ s) = s

-- | Extract the size of an hand
size :: Hand -> Int
size hand = length hand

