{- Card related data-types -}

module Cards where


-----------------------
------For testing -----
hand1 :: Hand
hand1 = [ Card (Num 2) Hearts, Card Jack Spades]

hand2 :: Hand
hand2 = [ Card (Num 2) Hearts, Card Jack Spades, Card (Num 5) Clubs]


hand3 :: Hand
hand3 = [ Card (Num 2) Spades, Card (Num 5) Clubs, Card (Num 2) Clubs, Card Jack Spades ]

-- | sorted
hand4 :: Hand
hand4 = [ Card (Num 2) Spades,  Card (Num 2) Clubs, Card (Num 2) Hearts, Card (Num 5) Clubs, Card Jack Spades ]

hand5 :: Hand
hand5 = [Card (Num 10) Hearts , Card Jack Hearts, Card Queen Hearts, Card King Hearts, Card Ace Hearts]

hand6 :: Hand
hand6 = [Card Queen Hearts, Card Queen Spades, Card King Hearts, Card King Spades, Card Ace Hearts ]
-----------------------
-----------------------


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



-- | TODO
-- *  Num int in data Rank should only be able to be 2-10

-- *  We need to be able to compare cards to sort them for the combinaton determination,
--    but if data Card have Ord, then both Rank and Suit should have Ord, but Suits does not carry any value i n texas hold em

-- * We might put a value to each of the ranks ( Num 10 is value 10, Jack is value 11 etc.). It 
--   might be easier to implement Straight in combination. It might also be useful if we want to
--   extend with other card-games later (the game "Casino" for example when the values is vital
--   for the game).