{- Card related data-types -}

data Suit = Hearts | Spades | Diamonds | Clubs
  deriving (Show)

data Rank = Ace | King | Queen | Jack | Num Int
  deriving (Show)

data Card = Card Rank Suit

instance Show Card where
  show (Card r s) = show r ++ " of " ++ show s
