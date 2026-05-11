

import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU
import Test.Tasty
import Test.Tasty (defaultMain)

import Game.UtilitiesTest
--import Game.StateTest

import TexasEngine
import Cards


--------------------- Main testing file -------------------------


main :: IO ()
main = defaultMain $ 
  testGroup "All test"
  [ Game.UtilitiesTest.tests
  ]


-- average' :: Float-> Float -> Float
-- average' a b = (a+b)/2

-- propAverage :: Float -> Float -> Bool
-- propAverage a b = average' a b == a/2 + b/2

-- tests :: TestTree
-- tests = testGroup "All test"
--   [ Game.UtilitiesTest.tests
--   ]



