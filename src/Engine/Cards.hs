module Engine.Cards 
    ( rank
    , suit
    , fullDeck
    , drawCard
    , shuffle
    , rankValue
    , rankValueAceLow
    )
    where

import Engine.EngineTypes

------------------------------------------------------------------------------------------
-- | Maps a Rank to its corresponding numerical value, with Ace high (14).
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

-- | Maps a rank to its corresponding numerical value, with Ace low (1).
--   This version is used when evaluating a low straight where Ace counts as 1
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


-- | Extract the rank of a card
rank :: Card -> Rank
rank (Card r _) = r

-- | Extract the suit of a card
suit :: Card -> Suit
suit (Card _ s) = s

------------------------------------------------------------------------------------------
-- | Creates a full 52-card deck in an unshuffuled order.
--   Built by combining every rank with every suit. Passed to 'shuffle' before dealing.
fullDeck :: Deck
fullDeck = [Card r s | r <- allRank, s <- allSuit]
    where
        allRank = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]
        allSuit = [Clubs, Diamonds, Hearts, Spades]


-- | Draws a card from the top of the deck, returning the card and the rest of the deck.
--   Returns Nothing if deck is empty.
drawCard :: Deck -> Maybe (Card, Deck)
drawCard [] = Nothing
drawCard (x:xs) = Just (x, xs)


-- | Removes the first occurence of a card from the deck.
--   Used by 'shuffle' to ensure the same card can't be picked twice.
removeCard:: Card -> Deck -> Deck
removeCard _ [] = []
removeCard x (y:ys)
    | x == y    = ys
    | otherwise = y : removeCard x ys


-- | Use a double between [0,1] to find a index. First card is picked
--   by 0.0 and the last card is picked by 1.0.
pick :: Double -> Deck -> Card
pick x d = d !! round ((fromIntegral (length d - 1)) * x)


-- | Produces a shuffled deck from a list of random Doubles and an input deck.
--   Each double is used to pick a card from the remianing deck and then removed
--   before the next card is picked, so that no duplicates can occur.
shuffle :: [Double] -> Deck -> Deck
shuffle _ [] = []
shuffle [] _ = []
shuffle (x:xs) d = card : shuffle xs d'
    where
        card  = pick x d
        d' = removeCard card d



