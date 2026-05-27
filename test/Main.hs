
import Test.Tasty
-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.HUnit as HU


import UtilitiesTest
import StateTest
import TestHandEvaluation



--------------------- Main testing file -------------------------


main :: IO ()
main = defaultMain $ 
  testGroup "All test"
  [ StateTest.unitTests,
    StateTest.propertyTests,
    UtilitiesTest.propertyTests,
    TestHandEvaluation.combinationsTests
  ]





