{- Card related data-types -}

module Cards where

import Types


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




--data Card = Card Rank Suit deriving (Eq, Ord)



--instance Show Card where
--  show (Card r s) = show r ++ show s



-- | Extract the rank of a card
rank :: Card -> Rank
rank (Card r _) = r

-- | Extract the suit of a card
suit :: Card -> Suit
suit (Card _ s) = s

