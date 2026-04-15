module Cards where

{- Card related data-types -}

import System.Random

import Types

-----------------------
------ For testing -----
hand1 :: Hand
hand1 = [ Card (Num 2) Hearts, Card Jack Spades]

hand2 :: Hand
hand2 = [ Card (Num 2) Hearts, Card Jack Spades, Card (Num 5) Clubs]

hand3 :: Hand
hand3 = [ Card (Num 2) Spades, Card (Num 5) Clubs, Card (Num 2) Clubs, Card Jack Spades ]

hand4 :: Hand
hand4 = [ Card (Num 2) Spades,  Card (Num 2) Clubs, Card (Num 2) Hearts, Card (Num 5) Clubs, Card Jack Spades ]

hand5 :: Hand
hand5 = [Card (Num 10) Hearts , Card Jack Hearts, Card Queen Hearts, Card King Hearts, Card Ace Hearts]

hand6 :: Hand
hand6 = [Card Queen Hearts, Card Queen Spades, Card King Hearts, Card King Spades, Card Ace Hearts ]
-----------------------
-----------------------
-- | Get functions for cards
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
-- | Help function for shuffle
-- | Removes the first card that matches a given card from the deck.
removeCard:: Card -> Deck -> Deck
removeCard _ [] = []
removeCard x (y:ys)
  | x == y    = ys
  | otherwise = y : removeCard x ys

-- |Help function for shuffle
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

-- | Help function for runShuffle
-- | Generate a list with random doubles
randomDoubles :: Int -> StdGen -> ([Double], StdGen)
randomDoubles 0 gen = ([], gen)
randomDoubles n gen = ((x:xs), gen2)
  where
    (x, gen1)  = randomR (0.0,1.0) gen
    (xs, gen2) = randomDoubles (n-1) gen1

-- | Create a new shuffled deck
runShuffle :: IO Deck
runShuffle = do
  gen <- newStdGen
  let (doubles, _) = randomDoubles 52 gen
  return (shuffle doubles fullDeck)

