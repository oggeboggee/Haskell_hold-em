{- Card related data-types -}

module Cards where


 -- | All different suits
data Suit = Hearts | Spades | Diamonds | Clubs deriving (Show, Eq, Ord)

-- | All different ranks
data Rank =  Num Int | Jack | Queen | King | Ace deriving (Show, Eq, Ord)

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
  show (Card r s) = " " ++ formatedRank r ++ " of " ++ formatedSuit s

formatedRank :: Rank -> String
formatedRank (Num n) = show n
formatedRank rank    = show rank

formatedSuit :: Suit -> String
formatedSuit s 
            | s == Spades = "\9824"
            | s == Hearts = "\9829"
            | s == Diamonds = "\9830"
            | s == Clubs = "\9827"

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

