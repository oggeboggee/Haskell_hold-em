module StateTest where


import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU
import Test.Tasty

import Control.Monad.State

import Types
import Cards
import TexasEngine
import Actions
import Utilities

-- =========================================================== --  
   ----------------------- Unit Tests ------------------------
-- | Group all unit tests
unitTests :: TestTree
unitTests = testGroup "Unit tests State"
    [ --unitConvertAction,
     --unitApllyEventPlayerEvent,
     propertyTests
    ]


-----------------------------------------------------
-- | Convert action unit tests
unitConvertAction :: TestTree
unitConvertAction = testGroup "converAction Unit tests"
    [ -- Small letter should work
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
unitApllyEventPlayerEvent :: TestTree
unitApllyEventPlayerEvent = testGroup "applyEvent Unit tests"
    [ 
    -- These test are using a predetermed state, see table1 at the bottom of the file under test cases

    ------------ Testing PlayerEvent Fold ------
    -- Player at index 0 in table1 is Bob, a fold should return a GameEvent, PlayerFolded "Bob"
    testCase "First player at table fold -> PlayerFolded <name_of_player>"
        $ (evalState (applyEvent (PlayerEvent 0 Fold)) table1) @?= Right [PlayerFolded "Bob"]

    , -- Compare how many have folded after one player have chooses to fold
    testCase "Number of folded players -> One more then before" 
        $ do
            let oldFoldedPlayers = filter folded (players table1)
                newState = execState (applyEvent (PlayerEvent 0 Fold)) table1
                newFoldedPlayers = filter folded (players newState)
            
            length newFoldedPlayers @?= length oldFoldedPlayers + 1



    --------------- Testing PlayerEvent Check -------
    , -- Player checking while not not matching highBet, should not be possible
    testCase "Player checking when behind highBet" 
        $ do
            let lastPlayerIndex = length (players table1) - 1
            (evalState (applyEvent (PlayerEvent lastPlayerIndex Check)) table1) 
                @?= Left "You can't check when behind the highbet."
            
    , -- A player who have matched hibet(either if highbet is 0 or player have bigBlind)
        -- is supposed to be able to check, big blind in this case is "Sam" at index 1
    testCase "Player checking while matching highBet"
        $ (evalState (applyEvent (PlayerEvent 1 Check)) table1) @?= Right [PlayerChecked "Sam"]
    

    --------------- Testing PlayerEvent Raise -------
    , -- Testing so that the bet transfer properly, testing with Jonathan at index 2 who currently are 100 behind highBet
        -- Raising should place the differens between his commited chips and the highBet + the amount to raise
    testCase "Player chips decrease after raising"
        $ do
            let chipsBefore = chips (players table1!!2) -- The amount of chips before raising
                bet         = 100
                highBetDif  = lowestBet table1 (players table1!!2) -- Amount to call/ Lowest bet 
                newState    = execState (applyEvent (PlayerEvent 2 (Raise bet))) table1 
                chipsAfter  = chips (players newState!!2)

            chipsAfter @?= (chipsBefore - highBetDif - bet)

    , -- Player raising invalid/negative amount 
    testCase "Negative raise amount"
        $ (evalState (applyEvent (PlayerEvent 2 (Raise (-100)))) table1) 
            @?= Left "Raise amount must be larger than 0 and smaller then the amount of chips you have"

    , -- Player raise with more chips than they currently have
    testCase "Rasie amount to high"
        $ (evalState (applyEvent (PlayerEvent 2 (Raise 100000000))) table1)
            @?= Left "Raise amount must be larger than 0 and smaller then the amount of chips you have"

    --------------- Testing PlayerEvent Call ---------
    -- , -- Call should place a bet so thatt the player match highBet
    -- testCase 
    ]


-- =========================================================== --  
   --------------------- Property Tests ----------------------

propertyTests :: TestTree
propertyTests = testGroup "Property tests State"
    [ 
    -- | A valid bet is bigger than zero an lower/equal than the amount of chips a player have
        -- An invalid bet should result in the left case and a valid in the right
    QC.testProperty "Valid/invalid raise amount" 
        $ QC.forAll (QC.choose(-1000, 3000))
            $ \x ->
            case (evalState (applyEvent (PlayerEvent 2 (Raise x))) table1) of
                Left "Raise amount must be larger than 0 and smaller then the amount of chips you have"
                    -> (x <= 0) || x > ((chips (players table1!!2)) - (lowestBet table1 (players table1!!2)))
                
                Right [PlayerRaised "Jonathan" x]
                    -> x > 0 && x < ((chips (players table1!!2)) - (lowestBet table1 (players table1!!2)))
    ]

-----------------------------------------------------





-- =========================================================== --  
   ----------------------- Test Cases ------------------------

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
