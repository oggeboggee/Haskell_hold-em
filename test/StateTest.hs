module StateTest where


import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU
import Test.Tasty

import Control.Monad.State

import Types
import Cards
import TexasEngine
import Actions

-- =============================================== --
------------------- Unit Tests ----------------------
-- | Group all unit tests
unitTests :: TestTree
unitTests = testGroup "Unit tests State"
    [ --unitConvertAction,
     unitApllyEvent
    ]


-----------------------------------------------------
-- | Convert action unit tests
unitConvertAction :: TestTree
unitConvertAction = testGroup "converAction Unit tests"
    [ -- Small letter works
    testCase "Convert Action 'fold' -> Just Fold" 
        $ convertAction "fold" @?= Just Fold

    , -- Capital letters should not matter
    testCase "Convert Action 'FOLD' -> Just Fold" 
        $ convertAction "FOLD" @?= Just Fold

    , -- Typos and gibberish should return Nothing
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



-----------------------------------------------------
-- | ApplyEvents Unit tests
unitApllyEvent :: TestTree
unitApllyEvent = testGroup "applyEvent Unit tests"
    [ -- Player at index 0 is Bob, a fold should return a GameEvent, PlayerFolded "Bob"
    testCase "First player at table fold -> PlayerFolded <name_of_player>"
        $ (evalState (applyEvent (PlayerEvent 0 Fold)) table1) @?= Right [PlayerFolded "Bob"]

    , -- Compare how many have folded after one player have chooses to fold
    testCase "Number of folded players -> One more then before" 
        $ do
            let oldFoldedPlayers = filter folded (players table1)
                newState = execState (applyEvent (PlayerEvent 0 Fold)) table1
                newFoldedPlayers = filter folded (players newState)
            
            length newFoldedPlayers @?= length oldFoldedPlayers + 1
    ]


-- =============================================== --
---------------- Property Tests ---------------------

propertyTests :: TestTree
propertyTests = testGroup "Property tests State"
    [ 
    ]

-----------------------------------------------------





-- =============================================== --
----------------- Exampel Cases ---------------------

--  Exampel state 1: 
-- In preflop, Sb -> Bob, Bb -> Sam
players1 :: [Player]
players1 = [Player "Bob" [] 950 50 False False,
                       Player "Sam" [] 900 100 False False,
                       Player "Jonathan" [] 1000 0 False False,
                       Player "Lewis" [] 1000 0 False False]

table1 :: Table
table1 = Table 
            { players = players1,
              deck = fullDeck,
              board = [],
              phase = PreFlop,
              highBet = 100,
              pot = 150,
              dealerPosition = 3,
              smallBlindPosition = 0,
              bigBlindPosition = 1,
              bets = []
            }
