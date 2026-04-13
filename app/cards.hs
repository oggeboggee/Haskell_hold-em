module App.Cards where

{- Card related data-types -}


-----------------------
------ For testing -----
hand1 :: Hand
hand1 = [ Card (Num 2) Hearts, Card Jack Spades]

card1 :: Card
card1 = Card (Num 2) Hearts

card2 :: Card
card2 = Card (Num 2) Spades

card3 :: Card
card3 = Card King Diamonds
-----------------------

 -- | All different suits
data Suit = Clubs | Diamonds | Hearts | Spades 
  deriving (Show, Eq, Ord)

-----------------------
-- | All different ranks
data Rank =  Num Int | Jack | Queen | King | Ace 
  deriving (Eq, Ord)

instance Show Rank where
  show r = case r of
    Num n -> show n
    Jack  -> "J"
    Queen -> "Q"
    King  -> "K"
    Ace   -> "A"

-----------------------
 -- | Card has an rank and a suit
data Card = Card Rank Suit 
  deriving (Eq)
-- | To compere two cards
instance Ord Card where
  compare (Card r1 s1) (Card r2 s2) = 
      if r1 == r2 then compare s1 s2 else compare r1 r2

 -- | to show the cards in a nice way
instance Show Card where
  show (Card r s) = " " ++ formatedRank r ++ {-" of " ++-} formatedSuit s

formatedRank :: Rank -> String
formatedRank (Num n) = show n
formatedRank rank    = show rank

formatedSuit :: Suit -> String
formatedSuit s 
            | s == Spades = "\9824"
            | s == Hearts = "\9829"
            | s == Diamonds = "\9830"
            | s == Clubs = "\9827"

-----------------------
 -- | Hand is a list of cards
type Hand = [Card]
 -- | Community Cards are the common card on the table
type CommunityCard = [Card]
-- | Deck is a list of cards
type Deck = [Card]

-----------------------
-- Functions --

-- | Get functions:
-- | Extract the rank of a card
rank :: Card -> Rank
rank (Card r _) = r

-- | Extract the suit of a card
suit :: Card -> Suit
suit (Card _ s) = s

-- | Extract the size of an hand
size :: Hand -> Int
size hand = length hand

-----------------------
-- | Creates a new deck with 52 cards
fullDeck :: Deck
fullDeck = [Card r s | r <- allRank, s <- allSuit]
  where
    allRank = [Num r | r <- [2..10]] ++ [Jack, Queen, King, Ace]
    allSuit = [Clubs, Diamonds, Hearts, Spades]

-- | Remove the first card from a deck
removeCard :: Deck -> Deck
removeCard []     = []
removeCard (x:xs) = xs

-- | If deck is not empty, draw a card from a deck and remove it from the deck
drawCard :: Deck -> (Card, Deck)
drawCard [] = error "Deck empty"
drawCard (x:xs) = (x, removeCard xs)

-- | Deal cards by splitting deck
dealCards :: Deck -> Int -> ([Card], Deck)
dealCards deck n = splitAt n deck



