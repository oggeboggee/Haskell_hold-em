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
    [ 
     unitConvertAction,
     unitApllyEventPlayerEvent,
     unitPlacePureBet
    ]

-- propertyTests :: TestTree
-- propertyTests = testGroup "Property tests State"
--     [ 
--      propertyTests
--     ]

-----------------------------------------------------
-- | Convert action unit tests
unitConvertAction :: TestTree
unitConvertAction = testGroup "converAction Unit tests"
    [ -- Small letter should work
    testCase "Convert Action 'fold'      -> Just Fold" 
        $ convertAction "fold" @?= Just Fold

    , -- Capital letters should not matter
    testCase "Convert Action 'FOLD'      -> Just Fold" 
        $ convertAction "FOLD" @?= Just Fold

    , -- Typos and gibberish should return Nothing
    testCase "Convert Action 'fodl'      -> Nothing" 
        $ convertAction "fodl" @?= Nothing

    , -- Extra whitespace gives invalid action
    testCase "Convert Action 'fold     ' -> Nothing"
        $ convertAction "fold     " @?= Nothing

    , -- | When raising, the digits after raise is converted correctly
    testCase "Convert Action 'raise 100' -> Just Rasie 100"
        $ convertAction "raise 100" @?= Just (Raise 100)

    , -- Raise must be followed by a digit
    testCase "Convert Action 'raise five' -> Nothing"
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
    testCase "First player at table fold -> PlayerFolded 'name'"
        $ (evalState (applyEvent (PlayerEvent 0 Fold)) table1) @?= Right [PlayerFolded "Bob"]

    , -- Compare how many have folded after one player have chooses to fold
    testCase "Number of folded players increase by one when folding" 
        $ do
            let oldFoldedPlayers = filter folded (players table1)
                newState = execState (applyEvent (PlayerEvent 0 Fold)) table1
                newFoldedPlayers = filter folded (players newState)
            
            length newFoldedPlayers @?= length oldFoldedPlayers + 1



    --------------- Testing PlayerEvent Check -------
    , -- Player checking while not not matching highBet, should not be possible
    testCase "Player unable to check when behind highBet" 
        $ do
            let lastPlayerIndex = length (players table1) - 1
            (evalState (applyEvent (PlayerEvent lastPlayerIndex Check)) table1) 
                @?= Left "You can't check when behind the highbet."
            
    , -- A player who have matched hibet(either if highbet is 0 or player have bigBlind)
        -- is supposed to be able to check, big blind in this case is "Sam" at index 1
    testCase "Player checking while matching highBet(bigblind or no highbet)"
        $ (evalState (applyEvent (PlayerEvent 1 Check)) table1) @?= Right [PlayerChecked "Sam"]
    

    --------------- Testing PlayerEvent Raise -------
    , -- Testing so that the bet transfer properly, testing with Jonathan at index 2 who currently are 100 behind highBet
        -- Raising should place the differens between his commited chips and the highBet + the amount to raise
    testCase "Player chips decrease correctly after raising"
        $ do
            let chipsBefore = chips (players table1!!2) -- The amount of chips before raising
                bet         = 100
                highBetDif  = lowestBet table1 (players table1!!2) -- Amount to call/ Lowest bet 
                newState    = execState (applyEvent (PlayerEvent 2 (Raise bet))) table1 
                chipsAfter  = chips (players newState!!2)

            chipsAfter @?= (chipsBefore - highBetDif - bet)


    -- Add a pot incease test
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
    -- testCase  "Call correct amount"
    --     $ do
    --         let chipsBefore    = chips (player table1!!2)
    --             expectedAmount = highBet table - commitedChips (player table1!!2)
    --             newState       = (execState (applyEvent (PlayerEvent 2 Call)) table1)
    --             chipsAfter     = chips (player newState!!2)
            
    --         expectedAmount @?= (chipsBefore - chipsAfter)


    ]


-----------------------------------------------------
-- | placePureBet Unit tests
unitPlacePureBet :: TestTree
unitPlacePureBet = testGroup "placePureBet Unit tests"
    [ -- placePureBet :: Table -> PlayerIndex -> Bet -> Table
        -- Using Jonathan at index 2 at table1 for exampel
    testCase "Player chips decrease when betting"
        $ let chipsBefore = chips (players table1!!2)
              bet         = 100
              table       = placePureBet table1 2 bet
              chipsAfter  = chips (players table!!2)

          in chipsAfter @?= (chipsBefore - bet)
    ,
    testCase "Table pot increase when a player bets"
        $ let potBefore = pot table1
              bet       = 100
              table     = placePureBet table1 2 bet
              potAfter  = pot table

          in potAfter @?= (potBefore + bet)

    ,
    testCase "Table higbet is uppdated when a player raise"
        $ let highBetBefore  = highBet table1
              bet            = 500
              table          = placePureBet table1 2 bet
              expectedHigbet = commitedChips (players table!!2)
              highBetAfter   = highBet table

          in highBetAfter @?= expectedHigbet

    ,
    testCase "Table higbet is NOT uppdated when a player call"
        $ let highBetBefore  = highBet table1
              bet            = 100
              table          = placePureBet table1 2 bet
              highBetAfter   = highBet table

          in highBetAfter @?= highBetAfter
    ]




-- =========================================================== --  
   --------------------- Property Tests ----------------------

propertyTests :: TestTree
propertyTests = testGroup "Property tests State"
    [ -- =================== Testing ApplyEvent ========================= --


    -- | A valid bet is bigger than zero and lower/equal than the amount of chips a player have 
        -- minus the amount they have to bet to match the current highBet
        -- An invalid bet should result in the left case and a valid in the right
    QC.testProperty "Valid/invalid raise amount" 
        $ QC.forAll (QC.choose(-1000, 3000))
            $ \x ->
            case (evalState (applyEvent (PlayerEvent 2 (Raise x))) table1) of
                Left "Raise amount must be larger than 0 and smaller then the amount of chips you have"
                    -> (x <= 0) || x > ((chips (players table1!!2)) - (lowestBet table1 (players table1!!2)))
                
                Right [PlayerRaised "Jonathan" x]
                    -> x > 0 && x < ((chips (players table1!!2)) - (lowestBet table1 (players table1!!2)))
    

    , -- Simulating a bettingphase where one player raise and set the highBet to a random amount 
        -- and the next player have a random amount of chips. The next player tries to check 
        -- -> should only work if higbBet != 0 and The player have enough chips to match the higBet
    QC.testProperty "Call when highBet is bigger than a players amount of chips"
        $ QC.forAll (QC.choose(0, 900)) $ \ x -> -- The amount of chips Lewis have
          QC.forAll (QC.choose(100, 900)) $ \ y -> -- y is the highBet set by Jonathan
                let players = [Player "Bob" [] 950 50 False False,
                               Player "Sam" [] 900 100 False False,
                               Player "Jonathan" [] (1000-y) y False False,
                               Player "Lewis" [] x 0 False False]
                    
                    state = Table -- Generating a state where Lewis may or may not have enough chips to call 
                                { players = players,
                                  deck = fullDeck,
                                  board = [],
                                  phase = PreFlop,
                                  highBet = y,
                                  pot = (150+y),
                                  dealerPosition = 3,
                                  smallBlindPosition = 0,
                                  bigBlindPosition = 1,
                                  bets = []
                                }

                    amount = lowestBet state (players!!3)

                in
                case (evalState (applyEvent (PlayerEvent 3 Call)) state) of
                    Left "There isn't a bet to call or you don't have enough chips to call."
                        -> highBet state == 0 || x < highBet state
                    
                    Right [PlayerCalled "Lewis" amount]
                        -> highBet state > 0  && x > highBet state
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



---------- Generate random game state -----------

{-
    How do I generate good random gmae states? Seems a little complicated.
    A GameState is dependent on events happaning???
     -- Is it possible to generate a fully random valid GameState
     -- Generating Cards
            --> No duplicates allowed
            --> How many cards are in play depend on gamephase

    -- Pot and player chips are dependent on previus betting history


    Would it be better to simply construct part of the state and only 
    randomly generate the parts that are interesting for the test.
    
-}


-- lenses





-- ================================================================= --
   ------------------------- Documentation -------------------------
--- One bug found so far
    -- Player could get negative chips when raising an amount close to their max chips
    -- Inbetween the amount of chips and lowest bet...

-- Another bug.. If someone can not match highBet, they can still call..
    -- And end up with negative chips
    -- Also need to change so that call don't show as an avalible action 



