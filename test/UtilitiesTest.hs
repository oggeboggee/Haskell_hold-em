{-|
This module contains contains test for some of the functions
that handle the cards being passed around in the game state.
-}

module UtilitiesTest where

import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU
import Test.Tasty
import Test.Tasty (defaultMain)


import Engine.TexasEngine
import Engine.Cards
import Engine.EngineTypes
import Engine.Utilities

import TestHelpers

------------- Testing pure utilites function ------------


propertyTests :: TestTree
propertyTests = testGroup "Property tests utility"
  [ QC.testProperty "Fulldeck --> 52 cards" propSizeFullDeck,
    QC.testProperty "Fulldeck --> No duplicates" propNoDup,
    
    QC.testProperty "nextPlayerToAct --> always give valid index" 
      -- Choose a random player at the table
      $ QC.forAll (QC.choose(0, (length (players table2) - 1))) $ \x -> 
        let nextIndex = (nextPlayerToAct x (players table2))
        in (nextIndex < (length (players table2))) && (nextIndex >= 0)
  ]

 -- ======================================================= --
 ----------------------- Unit tests --------------------------




 -- ======================================================= --
 -------------------- Properties tests -----------------------

-- | Size full deck
propSizeFullDeck :: Bool
propSizeFullDeck = length fullDeck == 52

-- | Check after duplicects in a full deck
propNoDup :: Bool
propNoDup = noDups fullDeck




