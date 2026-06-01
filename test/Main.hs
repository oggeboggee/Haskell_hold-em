{-|
This is the main test file. All test runs from here when running 'cabal test'.

The test suite uses Tasty, combining HUnit for unit tests, and QuickCheck for 
property test and random generation in order to generate random test data.
The suite is roughly devided in stateful (stateful adjacent) functions 
and pure logical functions. 

The purose of the test suite is to make sure that the game functions as expected. 
The test checks for know edge cases
-}


import Test.Tasty

import UtilitiesTest
import StateTest
import TestHandEvaluation



--------------------- Main testing file -------------------------

-- | The main test function that runs when running 'cabal test'
main :: IO ()
main = defaultMain $ 
  testGroup "All test"
  [ StateTest.unitTests,
    StateTest.propertyTests,
    UtilitiesTest.propertyTests,
    TestHandEvaluation.combinationsTests
  ]





