module Cards 
      (rank,
       suit,
       fullDeck,
       drawCard,
       shuffle,
       --runShuffle,
       rankValue,
       rankValueAceLow,
       hand1,
       hand2,
       hand3
      )
      where


{- Card related data-types -}


import Types

-----------------------
-- Functions --

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

rankValueAceLow :: Rank -> Int
rankValueAceLow r = case r of
  Ace   -> 1
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



--data Card = Card Rank Suit deriving (Eq, Ord)


--instance Show Card where
--  show (Card r s) = show r ++ show s


-- | Get functions:
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
    allRank = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]
    allSuit = [Clubs, Diamonds, Hearts, Spades]


---------------------------------------

-- | If deck is not empty, draw a card from a deck and remove it from the deck
drawCard :: Deck -> Maybe (Card, Deck)
drawCard [] = Nothing
drawCard (x:xs) = Just (x, xs)


-- | Deal cards by splitting deck
--dealCards :: Deck -> Int -> ([Card], Deck)
--dealCards deck n = splitAt n deck --needs to be shuffled, check if n is bigger than the length of deck.

--text

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
pick x dk = dk!!round ((fromIntegral (length dk - 1)) * x)

-- | Pick a random card using a double(for index) and put it in a new deck
    -- Note that the list of doubles need to be same or bigger length as deck
shuffle :: [Double] -> Deck -> Deck
shuffle _ [] = []
shuffle [] _ = []
shuffle (x:xs) dk = card : shuffle xs deck'
  where
    card  = pick x dk
    deck' = removeCard card dk

---------------------------------------
-- | Generate a list with random doubles, Help function for runShuffle
-- randomDoubles :: Int -> StdGen -> ([Double], StdGen)
-- randomDoubles 0 gen = ([], gen)
-- randomDoubles n gen = ((x:xs), gen2)
--   where
--     (x, gen1)  = randomR (0.0,1.0) gen
--     (xs, gen2) = randomDoubles (n-1) gen1

-- -- | Create a new shuffled deck
-- runShuffle :: IO Deck
-- runShuffle = do
--   gen <- newStdGen
--   let (doubles, _) = randomDoubles 52 gen
--   return (shuffle doubles fullDeck)


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

