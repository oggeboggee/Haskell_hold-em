module Game.UtilitiesTest (tests) where

import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU
import Test.Tasty
import Test.Tasty (defaultMain)


import TexasEngine
import Cards


------------- Testing pure utilites function ------------

tests :: TestTree
tests = testGroup "Utilites test"
  [ testProperty "propSizeFullDeck" propSizeFullDeck
  ]

-- | A new fullDeck should have 52 cards and no duplicates

-- | Size full deck
propSizeFullDeck :: Bool
propSizeFullDeck = length fullDeck == 52




-- propNoDup :: Deck -> Bool
-- propNoDup (x:xs) = noDups card (x:xs)

-- noDups :: Card -> Deck -> Bool
-- noDups card []     = True
-- noDups card (x:xs) = card != x && noDups card xs



