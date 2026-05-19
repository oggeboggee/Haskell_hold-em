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
import HandEvaluation

-- =========================================================== --  
   ----------------------- Unit Tests ------------------------
-- | Group all unit tests
unitTests :: TestTree
unitTests = testGroup "Unit tests State"
    [ 
     unitConvertAction,
     unitApllyEventPlayerEvent,
     unitPlacePureBet,
     unitPureFold,
     unitPureCheck
    ]

propertyTests :: TestTree
propertyTests = testGroup "Property tests State"
    [ 
     propertyTestsApplyEvent,
     propertyTestsPureFold,
     propertyTestsRunShowdown
    ]


-----------------------------------------------------
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
    , -- Call should place a bet so thatt the player match highBet
    testCase  "Call correct amount"
        $ do
            let chipsBefore    = chips (players table1!!2)
                expectedAmount = highBet table1 - commitedChips (players table1!!2)
                newState       = (execState (applyEvent (PlayerEvent 2 Call)) table1)
                chipsAfter     = chips (players newState!!2)
            
            expectedAmount @?= (chipsBefore - chipsAfter)


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


-----------------------------------------------------
-- | pureFold Unit tests
unitPureFold :: TestTree
unitPureFold = 
        let table       = pureFold table1 2
            playerAfter = players table!!2

        in
            testGroup "pureFold Unit tests" 
            [ -- When a aplyer ahve acted, acted should always be true untill a reset has happend
            testCase "Player folded bool change correctly"
                $ folded playerAfter @?= True
            
            , -- Check so that no other players acted state changes
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
-- | pureCheck Unit tests
unitPureCheck :: TestTree
unitPureCheck = 
        let table       = pureCheck table1 2
            playerAfter = players table!!2

        in
            testGroup "pureCheck Unit tests" 
            [ -- Correctly change the acted state when checking
            testCase "Player acted bool change correctly"
                $ acted playerAfter @?= True
            
            , -- Checking should not effect a players chips
            testCase "Player chips is unchanged when checking"
                $ 
                let chipsBefore = chips (players table1!!2)
                    chipsAfter  = chips (players table!!2)

                in chipsBefore @?= chipsAfter

            , -- Check that only one players acted state is changed
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
-- | runShowdown Unit tests

-- Testing with a fixed state
-- unitRunShowdown :: TestTree
-- unitRunShowdown =
--     let state = table2 {board = [Card Five Hearts, 
--                                  Card Six Spades, 
--                                  Card Seven Clubs, 
--                                  Card Eight Diamonds, 
--                                  Card Ace Hearts]}



-- =========================================================== --  
   --------------------- Property Tests ----------------------

propertyTestsApplyEvent :: TestTree
propertyTestsApplyEvent = testGroup "Property tests applyEvent"
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
    QC.testProperty "Call when highBet is possibly bigger than a players amount of chips"
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


    , -- Testing so that a player dont have negative chips after raising
    QC.testProperty "Players should never have negative chips after raising"
        $ QC.forAll (QC.choose(0, 1000000)) $ \x -> -- Amount to bet
            let state = execState (applyEvent (PlayerEvent 2 (Raise x))) table1
                
            in ((chips (players state!!2)) >= 0) && (noNegativeChips state)
            
    , -- Testing so that a player 
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
    [ -- Calculate the correct winners and compare to runShowdown calculation
    QC.testProperty "Calculate the correct winners" $
        QC.forAll (randomiseCardsAtTable table2 5) $ \table ->

            let activePlayers  = filter (not . folded) (players table)
                playerHands   = map hand activePlayers
                comCards      = board table
                winnerIndexes = winners comCards playerHands
                winnerPlayers = [activePlayers!!i | i <- winnerIndexes]
                winnerNames   = map name winnerPlayers

            in [ShowdownHappened winnerNames] == (evalState (runShowdown) table)

    --, -- Check so that the correct amount of money is dealt out to the correct players
    --QC.testProperty
    ]




-----------------------------------------------------
-- | properties pureFold


propertyTestsPureFold :: TestTree
propertyTestsPureFold = testGroup "Property tests pureFold"
    [  -- =================== Testing pureFold ========================= --
    QC.testProperty "When a player fold their fold bool should be True"
        -- Take a random player at the table and folds that player
        $ QC.forAll (QC.choose(0, (length (players table1) - 1))) $ \ x ->
            let table            = pureFold table1 x
                foldedplayer     = players table!!x

            in
                (folded foldedplayer) == True
    
    ,
    QC.testProperty "When a player fold their chips should remain the same"
        $ QC.forAll (QC.choose(0, (length (players table1) - 1))) $ \ x ->
            let beforeFold = players table1!!x
                table      = pureFold table1 x
                afterFold  = players table!!x

            in
                (chips beforeFold) == (chips afterFold)

    , -- This function should never be called on a player that have already folded
    QC.testProperty "When a player fold the number of folded players increase by one" 
        $ QC.forAll (QC.choose(0, (length (players table1) - 1))) $ \ x ->
            let table  = pureFold table1 x
                before = filter folded (players table1) -- Amount of folded player before 
                after  = filter folded (players table)  -- Amount of folded players after

            in
                (length after) == (length before + 1)

    ]






----------------------------------------------------------------------
-- Helper functions for checking so there is no negative values for chips, pot etc.
-- | Checks all player chips and table pot for negative
noNegativeChips :: Table -> Bool
noNegativeChips table = let allChipValues = (map chips (players table)) ++ [pot table]
                        in and (map (>=0) allChipValues)

-----------------------------------------------------





-- =========================================================== --  
   ----------------------- Test Cases ------------------------

--  Default state 1: 
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

----------
-- | Default state 2
-- | For showdown testing
players2 :: [Player]
players2 = [Player "Bob" [] 500 50 False False,
            Player "Sam" [] 500 100 False False,
            Player "Jonathan" [] 500 0 False False,
            Player "Lewis" [] 500 0 False False]

table2 :: Table
table2 = Table 
            { players = players1,
              deck = fullDeck,
              board = [],
              phase = Showdown,
              highBet = 0,
              pot = 600,
              dealerPosition = 3,
              smallBlindPosition = 0,
              bigBlindPosition = 1,
              bets = []
            }

--------------------------------------------------
---------- Generate random game state -----------

-- OBS! These generators can still produce duplicates

-- | Helper to generate random hands
randomiseHands :: [Player] -> QC.Gen [Player]
randomiseHands = mapM oneHand
    where
        oneHand player = do
            randomHand <- QC.vectorOf 2 (QC.elements fullDeck)
            pure player {hand = randomHand}

-- | Helper to generate random communityCards
randomiseCommunityCards :: Int -> QC.Gen [Card]
randomiseCommunityCards x = do
    communityCards <- QC.vectorOf x (QC.elements fullDeck)
    pure communityCards

-- | Helper to randomise all cards at a table
randomiseCardsAtTable :: Table -> Int -> QC.Gen Table
randomiseCardsAtTable table x = do
    newPlayers     <- randomiseHands (players table)
    communityCards <- randomiseCommunityCards x

    let allHands = [hand p | p <- newPlayers]
        allCards = communityCards ++ (concat allHands)

    if nuDupsHelper allCards 
        then pure $ table {players = newPlayers, board = communityCards}
    else 
        randomiseCardsAtTable table x




noDuplicatCards :: Table -> Bool
noDuplicatCards table = 
    let allHands = [hand p | p <- players table]
        allCards = board table ++ (concat allHands)
    
    in noDupsHelper allCards

noDupsHelper :: [Card] -> Bool
noDupsHelper []     = True
nuDupsHelper (x:xs) = x `elem` xs || noDupsHelper xs



------------------------------------------------------------------------




-- ================================================================= --
   ------------------------- Documentation -------------------------
--- One bug found so far
    -- Player could get negative chips when raising an amount close to their max chips
    -- Inbetween the amount of chips and lowest bet...

-- Another bug.. If someone can not match highBet, they can still call..
    -- And end up with negative chips
    -- Also need to change so that call don't show as an avalible action 




-- ================================================================= --
------------------------------ TODO --------------------------------
{-
 TestSuite
-- convertAction            [x]
-- applyEvent               [x] -- Could be done tested more
-- placePureBet             [x]
-- pureFold                 [x]
-- pureCheck                [x]
-- updatePlayerAtIndex      []
-- runShodown               []
        -- noNegativeChips
        -- correct winners
        -- Even pot distribution
        -- Chips increase correctly

-- NoNegativeChips          [x]
--------------------------------
Eninge
-- Waiting room             []
-- Player with 0 chips      []
-- Side pot                 []
-- Buy in                   []
-- No comCards when -        
    one players left        []

-----------------------------------
Documentation
-- Checklist                []
-- Future roadmap           []
-- Hadock                   []





-}