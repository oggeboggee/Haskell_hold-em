module Cards 
      (rank,
       suit,
       fullDeck,
       drawCard,
       shuffle,
       runShuffle,
       hand1,
       hand2,
       hand3)
      where

{- Card related data-types -}

import System.Random

import Types


--------------------------------------------------------------
--------------------------------------------------------------
-------------------- Get functions for cards -----------------
-- | Extract the rank of a card
rank :: Card -> Rank
rank (Card r _) = r

-- | Extract the suit of a card
suit :: Card -> Suit
suit (Card _ s) = s


--------------------------------------------------------------
--------------------------------------------------------------
-- | Creates a new deck with 52 cards
fullDeck :: Deck
fullDeck = [Card r s | r <- allRank, s <- allSuit]
  where
    allRank = [Two, Three, Four, Five, Six, Seven, 
                Eight, Nine, Ten, Jack, Queen, King, Ace]
    allSuit = [Clubs, Diamonds, Hearts, Spades]

---------------------------------------
-- | If deck is not empty, draw a card from a deck and remove it from the deck
drawCard :: Deck -> Maybe (Card, Deck)
drawCard [] = Nothing
drawCard (x:xs) = Just (x, xs)

---------------------------------------
-- | Help function for shuffle
-- | Removes the first card that matches a given card from the deck.
removeCard:: Card -> Deck -> Deck
removeCard _ [] = []
removeCard x (y:ys)
  | x == y    = ys
  | otherwise = y : removeCard x ys

-- | Use a double between 0-1 to find a index, Help function for shuffle
pick :: Double -> Deck -> Card
pick x deck = deck!!round ((fromIntegral (length deck - 1)) * x)

-- | Pick a random card using a double(for index) and put it in a new deck
    -- Note that the list of doubles need to be same or bigger length as deck
shuffle :: [Double] -> Deck -> Deck
shuffle _ [] = []
shuffle (x:xs) deck = card : shuffle xs deck'
  where
    card  = pick x deck
    deck' = removeCard card deck

---------------------------------------
-- | Generate a list with random doubles, Help function for runShuffle
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
--------------------------------------------------------------
--------------------------------------------------------------

--------------------------------------------------------------
------------------------ For testing -------------------------
hand1 :: Hand
hand1 = [ Card Two Hearts, Card Jack Spades]

hand2 :: Hand
hand2 = [ Card Two Diamonds, Card Jack Spades]

hand3 :: Hand
hand3 = [ Card Ten Spades, Card King Clubs]

-- hand4 :: Hand
-- hand4 = [ Card Two Spades,  Card Two Clubs, Card Two Hearts, Card Five Clubs, Card Jack Spades ]

-- hand5 :: Hand
-- hand5 = [Card Ten Hearts , Card Jack Hearts, Card Queen Hearts, Card King Hearts, Card Ace Hearts]

-- hand6 :: Hand
-- hand6 = [Card Queen Hearts, Card Queen Spades, Card King Hearts, Card King Spades, Card Ace Hearts ]

