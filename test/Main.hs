---------------------- Main testing file -------------------------

import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU
import Test.Tasty
import Test.Tasty (defaultMain)




main :: IO ()
main = defaultMain tests


average' :: Float-> Float -> Float
average' a b = (a+b)/2

propAverage :: Float -> Float -> Bool
propAverage a b = average' a b == a/2 + b/2

tests :: TestTree
tests = testGroup "My Tests"
  [ testProperty "prop_name" propAverage
  ]



