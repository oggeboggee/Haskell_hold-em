{-|
This module contains all tests for the stateful functions, 
plus some of the adjacent helper functions.

The tests are divided in two parts, 
Unit tests (HUnit) and property test(QuickCheck). 

For the stateful test we use partialy randomised table states to test.
We start by using a predetermend state and then randomise the parts 
that we are interested in. The predetermend states and the functions
randomising parts of the state can be found in TestHelpers.hs
-}

module StateTest where

import Control.Exception
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU
import Test.Tasty

import Control.Monad.State

import Engine.EngineTypes
import Engine.Cards
import Engine.TexasEngine
import Engine.Actions
import Engine.Utilities
import Engine.HandEvaluation

import TestHelpers
-- =========================================================== --  
   ----------------------- Unit Tests ------------------------
-- | Group all unit tests in StateTest
unitTests :: TestTree
unitTests = testGroup "Unit tests State"
    [ 
     --unitConvertAction,
     unitApllyEventPlayerEvent,
     unitPlacePureBet,
     unitPureFold,
     unitPureCheck,
     unitRunShowdown,
     unitUpdatePlayerAtIndex
    ]

-- | Group all property tests in StateTest
propertyTests :: TestTree
propertyTests = testGroup "Property tests State"
    [
     propertyTestsApplyEvent,
     propertyTestsPureFold,
     propertyTestsRunShowdown,
     propertyTestsPureCheck
    ]


-----------------------------------------------------
-----------------------------------------------------
-- This function was used in the Terminal version of the game, but are no longer used
-- | Convert action unit tests
-- unitConvertAction :: TestTree
-- unitConvertAction = testGroup "converAction Unit tests"
--     [ -- Small letter should work
--     testCase "Convert Action 'fold'      -> Just Fold" 
--         $ convertAction "fold" @?= Just Fold

--     , -- Capital letters should not matter
--     testCase "Convert Action 'FOLD'      -> Just Fold" 
--         $ convertAction "FOLD" @?= Just Fold

--     , -- Typos and gibberish should return Nothing
--     testCase "Convert Action 'fodl'      -> Nothing" 
--         $ convertAction "fodl" @?= Nothing

--     , -- Extra whitespace gives invalid action
--     testCase "Convert Action 'fold     ' -> Nothing"
--         $ convertAction "fold     " @?= Nothing

--     , -- | When raising, the digits after raise is converted correctly
--     testCase "Convert Action 'raise 100' -> Just Rasie 100"
--         $ convertAction "raise 100" @?= Just (Raise 100)

--     , -- Raise must be followed by a digit
--     testCase "Convert Action 'raise five' -> Nothing"
--         $ convertAction "raise five" @?= Nothing
--     ]



-----------------------------------------------------
-- | Unit tests for ApplyEvent
-- These test are using a predetermed state
unitApllyEventPlayerEvent :: TestTree
unitApllyEventPlayerEvent = testGroup "applyEvent Unit tests"
    [ 
    ------------ Testing PlayerEvent Fold ------
    -- | Player at index 0 in table1 is Bob, a fold should return a GameEvent, PlayerFolded "Bob"
    testCase "First player at table fold -> PlayerFolded 'name'"
        $ (evalState (applyEvent (PlayerEvent 0 Fold)) table1) @?= Right [PlayerFolded "Bob"]

    , -- | Compare how many have folded after one player have chooses to fold
    testCase "Number of folded players increase by one when folding" 
        $ do
            let oldFoldedPlayers = filter folded (players table1)
                newState = execState (applyEvent (PlayerEvent 0 Fold)) table1
                newFoldedPlayers = filter folded (players newState)
            
            length newFoldedPlayers @?= length oldFoldedPlayers + 1

    --------------- Testing PlayerEvent Check -------
    , -- | Player checking while not not matching highBet, should not be possible
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
    , -- | Testing so that the bet transfer properly, testing with Jonathan at index 2 who currently are 100 behind highBet
      -- Raising should place the differens between his commited chips and the highBet + the amount to raise
    testCase "Player chips decrease correctly after raising"
        $ do
            let chipsBefore = chips (players table1!!2) -- The amount of chips before raising
                bet         = 100
                highBetDif  = lowestBet table1 (players table1!!2) -- Amount to call/ Lowest bet 
                newState    = execState (applyEvent (PlayerEvent 2 (Raise bet))) table1 
                chipsAfter  = chips (players newState!!2)

            chipsAfter @?= (chipsBefore - highBetDif - bet)


      -- | Add a pot incease test
    , -- Player raising invalid/negative amount 
    testCase "Negative raise amount"
        $ (evalState (applyEvent (PlayerEvent 2 (Raise (-100)))) table1) 
            @?= Left "Raise amount must be larger than 0 and smaller then the amount of chips you have"

    , -- | Player raise with more chips than they currently have
    testCase "Rasie amount to high"
        $ (evalState (applyEvent (PlayerEvent 2 (Raise 100000000))) table1)
            @?= Left "Raise amount must be larger than 0 and smaller then the amount of chips you have"

    --------------- Testing PlayerEvent Call ---------
    , -- | Call should place a bet so thatt the player match highBet
    testCase  "Call correct amount"
        $ do
            let chipsBefore    = chips (players table1!!2)
                expectedAmount = highBet table1 - commitedChips (players table1!!2)
                newState       = (execState (applyEvent (PlayerEvent 2 Call)) table1)
                chipsAfter     = chips (players newState!!2)
            
            expectedAmount @?= (chipsBefore - chipsAfter)

    ]
-----------------------------------------------------


-----------------------------------------------------
-- | placePureBet Unit tests using predetermend States
unitPlacePureBet :: TestTree
unitPlacePureBet = testGroup "placePureBet Unit tests"
    [ -- | Using 'Jonathan' at index 2 at table1 for exampel
    testCase "Player chips decrease when betting"
        $ let chipsBefore = chips (players table1!!2)
              bet         = 100
              table       = placePureBet table1 2 bet
              chipsAfter  = chips (players table!!2)

          in chipsAfter @?= (chipsBefore - bet)
    , -- | Testing that the pot at the table increase correctly when betting
    testCase "Table pot increase when a player bets"
        $ let potBefore = pot table1
              bet       = 100
              table     = placePureBet table1 2 bet
              potAfter  = pot table

          in potAfter @?= (potBefore + bet)

    , -- | Testing that the highBet at the table is updated correctly when a player raise
    testCase "Table higbet is updated when a player raise"
        $ let --highBetBefore  = highBet table1
              bet            = 500
              table          = placePureBet table1 2 bet
              expectedHigbet = commitedChips (players table!!2)
              highBetAfter   = highBet table

          in highBetAfter @?= expectedHigbet

    , -- | Testing that the highBet stays the same when a player calls
    testCase "Table higbet is NOT uppdated when a player call"
        $ let --highBetBefore  = highBet table1
              bet            = 100
              table          = placePureBet table1 2 bet
              highBetAfter   = highBet table

          in highBetAfter @?= highBetAfter
    ]
-----------------------------------------------------

-----------------------------------------------------
-- | pureFold Unit tests
unitPureFold :: TestTree
unitPureFold = 
        let table       = pureFold table1 2
            playerAfter = players table!!2

        in
            testGroup "pureFold Unit tests" 
            [ -- | When a aplyer have acted, acted should always be true untill a reset has happend
            testCase "Player folded bool change correctly"
                $ folded playerAfter @?= True
            
            , -- | Check so that no other players acted state changes
            testCase "No other player have change their folded state"
                $   
                let otherPlayersBefore = (take 2 (players table1) ++ drop 3 (players table1))
                    otherPlayersAfter  = (take 2 (players table) ++ drop 3 (players table))
                    
                in and [folded b == folded a | 
                        b <- otherPlayersBefore, 
                        a <- otherPlayersAfter] 
                        @?= True
            ]
-----------------------------------------------------

-----------------------------------------------------
-- | pureCheck Unit tests
unitPureCheck :: TestTree
unitPureCheck = 
        let table       = pureCheck table1 2
            playerAfter = players table!!2

        in
            testGroup "pureCheck Unit tests" 
            [ -- | Correctly change the acted state when checking
            testCase "Player acted bool change correctly"
                $ acted playerAfter @?= True
            
            , -- | Checking should not effect a players chips
            testCase "Player chips is unchanged when checking"
                $ 
                let chipsBefore = chips (players table1!!2)
                    chipsAfter  = chips (players table!!2)

                in chipsBefore @?= chipsAfter

            , -- | Check that only one players acted state is changed
            testCase "No other player have change their acted state"
                $   
                let otherPlayersBefore = (take 2 (players table1) ++ drop 3 (players table1))
                    otherPlayersAfter  = (take 2 (players table) ++ drop 3 (players table))
                    
                in and [acted b == acted a | 
                        b <- otherPlayersBefore, 
                        a <- otherPlayersAfter] 
                        @?= True
            ]
    
-----------------------------------------------------

-----------------------------------------------------
-- | runShowdown Unit tests

unitRunShowdown :: TestTree
unitRunShowdown = testGroup "Unit test runShowdown"
    [ -- | Calculate winners, using table3 winners should be Sam and Jonathan
    testCase "Correct winners"
        $   let showdownHappend = (evalState (runShowdown) table3)
                winnersName     = unPackWinners (showdownHappend!!0)

            in winnersName @?= ["Sam", "Jonathan"]
    
    , -- | Both players should get 300 chips
    testCase "Correct dealout of chips to winners first"
        $   let finalState          = (execState (runShowdown) table3)
                samChipsBefore      = chips (players table3!!1)
                --jonathanChipsBefore = chips (players table3!!2)

                samChipsAfter       = chips (players finalState!!1)
                --jonathanChipsAfter  = chips (players finalState!!2)
            
            in samChipsAfter @?= (samChipsBefore + 300)
    
    , -- | Both players should get 300 chips
    testCase "Correct dealout of chips to winners second"
        $   let finalState          = (execState (runShowdown) table3)
                jonathanChipsBefore = chips (players table3!!2)
                jonathanChipsAfter  = chips (players finalState!!2)
            
            in jonathanChipsAfter @?= (jonathanChipsBefore + 300)

    , -- | Pot is set to 0
    testCase "Pot is set to zero"
        $   let finalState = (execState (runShowdown) table3)

            in (pot finalState) @?= 0
    ]
    

-----------------------------------------------------
-- | runShowdown Unit tests

unitUpdatePlayerAtIndex :: TestTree
unitUpdatePlayerAtIndex = testGroup "Unit test updatePlayerAtIndex"
    [ -- | 10 is a invalid index 
    testCase "Index of out range give error"
        $   assertThrows 
            (evaluate 
                (updatePlayerAtIndex 100 (\p -> p { acted = True }) table1)) 
                "Invalid index in updatePlayerAtIndex"

    , -- | updatePlayerAtIndex should not be able to take a negative index
    testCase "Negative index should give an error"
        $   assertThrows 
            (evaluate 
                (updatePlayerAtIndex (-1) (\p -> p { acted = True }) table1)) 
                "Invalid index in updatePlayerAtIndex"
        
    , -- | Testing that all the other players are still the same
    testCase "Only one player should change their state (first player in list)"
        $   let playerBefore = tail (players table1)
                finalState   = updatePlayerAtIndex 0 (\p -> p { acted = True }) table1
                playersAfter = tail (players finalState)
            
            in playerBefore @?= playersAfter

    , -- | Testing that all the other players are still the same
    testCase "Only one player should change their state (last player in list)"
        $   let playerBefore = (tail . reverse) (players table1)
                index        = (length (players table1) - 1)
                finalState   = updatePlayerAtIndex index (\p -> p { acted = True }) table1
                playersAfter = (tail . reverse) (players finalState)
            
            in playerBefore @?= playersAfter
    ]

-- =========================================================== --  




-- =========================================================== --  
   --------------------- Property Tests ----------------------

-- | Property test for Apply Event

propertyTestsApplyEvent :: TestTree
propertyTestsApplyEvent = testGroup "Property tests applyEvent"
    [ -- =================== Testing ApplyEvent ========================= --


    -- | A valid bet is bigger than zero and lower/equal than the amount of chips a player have 
    -- minus the amount they have to bet to match the current highBet
    -- An invalid bet should result in the left case and a valid in the right
    QC.testProperty "Valid/invalid raise amount" 
        $ QC.forAll (QC.choose(-1000, 3000))
            $ \r ->
            case (evalState (applyEvent (PlayerEvent 2 (Raise r))) table1) of
                Left "Raise amount must be larger than 0 and smaller then the amount of chips you have"
                    -> (r <= 0) || r > ((chips (players table1!!2)) - (lowestBet table1 (players table1!!2)))
                
                Right [PlayerRaised "Jonathan" r]
                    -> r > 0 && r <= ((chips (players table1!!2)) - (lowestBet table1 (players table1!!2)))


    , -- | Simulating a bettingphase where one player raise and set the highBet to a random amount 
      -- and the next player have a random amount of chips. The next player tries to check 
      -- -> should only work if higbBet != 0 and The player have enough chips to match the higBet
    QC.testProperty "Call when highBet is possibly bigger than a players amount of chips"
        $ QC.forAll (QC.choose(0, 900)) $ \ x -> -- The amount of chips Lewis have
          QC.forAll (QC.choose(100, 900)) $ \ y -> -- y is the highBet set by Jonathan
                let pl = [Player "Bob" [] 950 50 False False,
                          Player "Sam" [] 900 100 False False,
                          Player "Jonathan" [] (1000-y) y False False,
                          Player "Lewis" [] x 0 False False]
                    
                    finalState = Table -- Generating a state where Lewis may or may not have enough chips to call 
                                { players = pl,
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

                    amount = lowestBet finalState (pl!!3)

                in
                case (evalState (applyEvent (PlayerEvent 3 Call)) finalState) of
                    Left "There isn't a bet to call or you don't have enough chips to call."
                        -> highBet finalState == 0 || x <= highBet finalState
                    
                    Right [PlayerCalled "Lewis" amount]
                        -> highBet finalState > 0  && x >= highBet finalState


    , -- | Testing so that a player dont have negative chips after raising
    QC.testProperty "Players should never have negative chips after raising"
        $ QC.forAll (QC.choose(0, 1000000)) $ \x -> -- Amount to bet
            let finalState = execState (applyEvent (PlayerEvent 2 (Raise x))) table1
                
            in ((chips (players finalState!!2)) >= 0) && (noNegativeChips finalState)
            
    , -- | Testing so that a player have zero chips after going all in
    QC.testProperty "After AllIn a players chips should always be 0"
        $ QC.forAll (QC.choose(0, 1000000)) $ \x -> -- Amount to bet
            let state1     = updatePlayerAtIndex 2 (\p -> p {chips = x}) table1
                finalState = execState (applyEvent (PlayerEvent 2 AllIn)) state1

            in ((chips (players finalState!!2)) == 0) && (noNegativeChips finalState)

    ]           


-----------------------------------------------------
-- | properties runShowdown


propertyTestsRunShowdown :: TestTree
propertyTestsRunShowdown = testGroup "Property tests runShowdown"
    [ -- | Calculate the correct winners and compare to runShowdown calculation
    QC.testProperty "Calculate the correct winners" $
        QC.forAll (randomiseCardsAtTable table2 5) $ \table ->

            let activePlayers  = filter (not . folded) (players table)
                playerHands   = map hand activePlayers
                comCards      = board table
                winnerIndexes = winners comCards playerHands
                winnerPlayers = [activePlayers!!i | i <- winnerIndexes]
                winnerNames   = map name winnerPlayers

            in winnerNames == (unPackWinners ((evalState (runShowdown) table)!!0))

    , -- | Check so that the correct amount of money is dealt out to the correct players
    QC.testProperty "The winners chips increase after winning" $
        QC.forAll (randomiseCardsAtTable table2 5) $ \table ->
            

            let (showdown, finalState) = (runState (runShowdown) table)
                winnersNames = unPackWinners (showdown!!0)

                winnersBefore = [ p | p <- players table, n <- winnersNames, name p == n]
                winnersAfter  = [ p | p <- players finalState, n <- winnersNames, name p == n]
            
            in and [chips pa > chips pb | pa <- winnersAfter, pb <- winnersBefore]
    
    , -- | Testing that runShowdown returns the correct winners
    QC.testProperty "The winners wins the correct amount of chips" $
        QC.forAll (randomiseCardsAtTable table2 5) $ \table ->
        QC.forAll (QC.choose (0, 100000)) $ \x ->

            let initTable = table {pot = x}
                (showdown, finalState) = (runState (runShowdown) initTable)
                winnersNames = unPackWinners (showdown!!0)

                winnersBefore = [ p | p <- players initTable, n <- winnersNames, name p == n]
                winnersAfter  = [ p | p <- players finalState, n <- winnersNames, name p == n]
            
                chipsWon = (pot initTable) `div` (length winnersNames)

            in and [chips pa == (chips pb + chipsWon) | pa <- winnersAfter, pb <- winnersBefore]

    , -- | Testing so there are no negative chips after showdown happend
    QC.testProperty "No negative chips after runShowdown" $
        QC.forAll (randomiseCardsAtTable table2 5) $ \table ->
        QC.forAll (QC.choose (0, 100000)) $ \x -> 

            let initTable = table {pot = x}
                finalState = (execState (runShowdown) initTable)

            in noNegativeChips finalState

    , -- | The pot should be emptied after showdown happend.
    QC.testProperty "Pot should be emptyt after showdown happend" $
        QC.forAll (randomiseCardsAtTable table2 5) $ \table ->
        QC.forAll (QC.choose (0, 100000)) $ \x -> 

            let initTable = table {pot = x}
                finalState = (execState (runShowdown) initTable)

            in (pot finalState) == 0
    
    ]




-----------------------------------------------------
-- | properties pureFold


propertyTestsPureFold :: TestTree
propertyTestsPureFold = testGroup "Property tests pureFold"
    [  -- =================== Testing pureFold ========================= --
    QC.testProperty "When a player fold their fold bool should be True"
        -- | Take a random player at the table and folds that player
        $ QC.forAll (QC.choose(0, (length (players table1) - 1))) $ \ x ->
            let table            = pureFold table1 x
                foldedplayer     = players table!!x

            in
                (folded foldedplayer) == True
    
    , -- | Chips are uneffected by folding
    QC.testProperty "When a player fold their chips should remain the same"
        $ QC.forAll (QC.choose(0, (length (players table1) - 1))) $ \ x ->
            let beforeFold = players table1!!x
                table      = pureFold table1 x
                afterFold  = players table!!x

            in
                (chips beforeFold) == (chips afterFold)

    , -- | When a player folds the number of folded players incease by one
      -- Note! This function should never be called on a player that have already folded
    QC.testProperty "When a player fold the number of folded players increase by one" 
        $ QC.forAll (QC.choose(0, (length (players table1) - 1))) $ \ x ->
            let table  = pureFold table1 x
                beforeFold = filter folded (players table1) -- Amount of folded player before 
                afterFold  = filter folded (players table)  -- Amount of folded players after

            in
                (length afterFold) == (length beforeFold + 1)
    

    ]

-----------------------------------------------------
-- | properties pureCheck


propertyTestsPureCheck :: TestTree
propertyTestsPureCheck = testGroup "Property tests pureCheck"
    [  -- =================== Testing pureFold ========================= --
    QC.testProperty "When a player check their acted state should update"
        -- | Take a random player at the table and checks, should mean acted is true
        -- Note that a player should only be able to check i certain situations but pureCheck dont care about that
        $ QC.forAll (QC.choose(0, (length (players table1) - 1))) $ \ x ->
            let table            = pureCheck table1 x
                checkdPlayer     = players table!!x

            in
                (acted checkdPlayer) == True

    , -- | Checking means no bet is placed, meaning a players chips should remain the same
    QC.testProperty "When a player fold their chips should remain the same"
        $ QC.forAll (QC.choose(0, (length (players table1) - 1))) $ \ x ->
            let beforeCheck = players table1!!x
                table      = pureCheck table1 x
                afterCheck  = players table!!x

            in
                (chips beforeCheck) == (chips afterCheck)
    ]

