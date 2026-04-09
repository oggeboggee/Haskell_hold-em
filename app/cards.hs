{- Card related data-types -}


-----------------------
------For testing -----
hand1 :: Hand
hand1 = [ Card (Num 2) Hearts, Card Jack Spades]
-----------------------


 -- | All different suits
data Suit = Hearts | Spades | Diamonds | Clubs deriving (Show, Eq)

-- | All different ranks
data Rank =  Num Int | Jack | Queen | King | Ace deriving (Show, Eq)

 -- | Card has an rank and a suit
data Card = Card Rank Suit deriving (Eq)

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
