module App.Cards where

import System.Random
-- import QuickCheck
-----------------------
------ For testing -----
hand1 :: Hand
hand1 = [ Card (Num 2) Hearts, Card Jack Spades]

hand2 :: Hand
hand2 = [ Card (Num 6) Hearts, Card Ace Diamonds]

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

----------------------------------------------
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

----------------------------------------------
-- | Creates a new deck with 52 cards
fullDeck :: Deck
fullDeck = [Card r s | r <- allRank, s <- allSuit]
  where
    allRank = [Num r | r <- [2..10]] ++ [Jack, Queen, King, Ace]
    allSuit = [Clubs, Diamonds, Hearts, Spades]


-- | If deck is not empty, draw a card from a deck and remove it from the deck
drawCard :: Deck -> Maybe (Card, Deck)
drawCard [] = Nothing
drawCard (x:xs) = Just (x, xs)


----------------------------------------------
-- | Removes the first card that matches a given card from the deck.
removeCard:: Card -> Deck -> Deck
removeCard _ [] = []
removeCard x (y:ys)
  | x == y    = ys
  | otherwise = y : removeCard x ys

-- |Use a double between 0-1 to find a index 
pick :: Double -> Deck -> Card
pick x deck = deck !! round ((fromIntegral (length deck - 1)) * x)

-- | Pick a random card using a double(for index) and put it in a new deck
shuffle :: [Double] -> Deck -> Deck
shuffle _ [] = []
shuffle (x:xs) deck = card : shuffle xs deck'
  where
    card  = pick x deck
    deck' = removeCard card deck

-- | Generate a list with random doubles
randomDoubles :: Int -> StdGen -> ([Double], StdGen)
randomDoubles 0 gen = ([], gen)
randomDoubles n gen = ((x:xs), gen2)
  where
    (x, gen1)  = randomR (0.0,1.0) gen
    (xs, gen2) = randomDoubles (n-1) gen1

-- | Create a new random genarator and shuffle a full deck
runShuffle :: IO Deck
runShuffle = do
  gen <- newStdGen
  let (doubles, _) = randomDoubles 52 gen
  return (shuffle doubles fullDeck)


