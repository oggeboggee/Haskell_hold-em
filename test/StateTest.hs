module StateTest where


import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU
import Test.Tasty

import Types
import Cards
import TexasEngine
import Actions

---------------- Unit Tests ---------------------
-- | Group all unit tests
unitTests :: TestTree
unitTests = testGroup "Unit tests State"
    [ unitConvertAction
    ]



-- | Convert action unit tests
unitConvertAction :: TestTree
unitConvertAction = testGroup "converAction Unit tests"
    [ -- Small letter works
    testCase "Convert Action 'fold' -> Just Fold" 
        $ convertAction "fold" @?= Just Fold

    , -- Capital letters should not matter
    testCase "Convert Action 'FOLD' -> Just Fold" 
        $ convertAction "FOLD" @?= Just Fold

    , -- Typo in string should give an invalid action error
    testCase "Convert Action 'fodl' -> Nothing" 
        $ convertAction "fodl" @?= Nothing

    , -- Extra whitespace gives invalid action
    testCase "Convert Action 'fold     '(whitespace)"
        $ convertAction "fold     " @?= Nothing

    , -- | When raising, the digits after raise is converted
    testCase "Convert Action 'raise 100' -> Just Rasie 100"
        $ convertAction "raise 100" @?= Just (Raise 100)

    , -- Raise must be followed by a digit
    testCase "Convert Action 'raise five' -> Just Rasie 100"
        $ convertAction "raise five" @?= Nothing
    ]
---------------- Property Tests ---------------------

propertyTests :: TestTree
propertyTests = testGroup "Property tests State"
    [ 
    ]

