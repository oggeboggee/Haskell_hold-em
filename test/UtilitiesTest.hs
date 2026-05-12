module UtilitiesTest where

import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU
import Test.Tasty
import Test.Tasty (defaultMain)


import TexasEngine
import Cards
import Types


------------- Testing pure utilites function ------------

tests :: TestTree
tests = testGroup "Utilites test"
  [ testProperty "propSizeFullDeck" propSizeFullDeck,
    testProperty "propNoDup" propNoDup
  ]

-- | A new fullDeck should have 52 cards and no duplicates

-- | Size full deck
propSizeFullDeck :: Bool
propSizeFullDeck = length fullDeck == 52

-- | Check after duplicects in a full deck
propNoDup :: Bool
propNoDup = noDups fullDeck

-- | Helper function, Return true if there are No duplicates
noDups :: Deck -> Bool
noDups []     = True
noDups (x:xs) = not (x `elem` xs) || noDups xs




