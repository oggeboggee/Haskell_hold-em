module TestHelpers where

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




--------------------------------------------------
---------- Generate random game state -----------

-- OBS! Some of these generators can still produce duplicates

---------------
-- | Helper to generate random hands
randomiseHands :: [Player] -> QC.Gen [Player]
randomiseHands = mapM oneHand
    where
        oneHand player = do
            randomHand <- QC.vectorOf 2 (QC.elements fullDeck)
            pure player {hand = randomHand}

---------------
-- | Helper to generate random communityCards
randomiseCommunityCards :: Int -> QC.Gen [Card]
randomiseCommunityCards x = do
    communityCards <- QC.vectorOf x (QC.elements fullDeck)
    pure communityCards

--------------
-- | Helper to randomise all cards at a table
randomiseCardsAtTable :: Table -> Int -> QC.Gen Table
randomiseCardsAtTable table x = do
    newPlayers     <- randomiseHands (players table)
    communityCards <- randomiseCommunityCards x

    let allHands = [hand p | p <- newPlayers]
        allCards = communityCards ++ (concat allHands)

    if noDups allCards 
        then pure $ table {players = newPlayers, board = communityCards}
    else 
        randomiseCardsAtTable table x



--------------------------------------------------------------------
noDuplicatCards :: Table -> Bool
noDuplicatCards table = 
    let allHands = [hand p | p <- players table]
        allCards = board table ++ (concat allHands)
    
    in noDups allCards


noDups :: [Card] -> Bool
noDups []     = True
noDups (x:xs) = if x `elem` xs then False else noDups xs

unPackWinners :: GameEvent -> [PlayerName]
unPackWinners (ShowdownHappened pl) = pl
unPackWinners _                     = []


-- Helper functions for checking so there is no negative values for chips, pot etc.
-- | Checks all player chips and table pot for negative
noNegativeChips :: Table -> Bool
noNegativeChips table = let allChipValues = (map chips (players table)) ++ [pot table]
                        in and (map (>=0) allChipValues)



-- =========================================================== --  
   ----------------------- Error handler ------------------------

assertThrows :: IO a -> String -> Assertion
assertThrows action expectedMessage = do
    result <- try action

    case result of
        Left (ErrorCall msg) -> msg @?= expectedMessage

        Right _ -> assertFailure "Expected exception but none was thrown"





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
            { players = players2,
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


-- This faild in "Winners win correct amount of chips"
-- DO NOT CHANGE, is used to test runShowdown in unit tests
players3 :: [Player]
players3 = [Player "Bob" [Card Queen Diamonds, Card Jack Spades] 500 50 False False,
            Player "Sam" [Card Two Spades, Card Three Hearts] 500 100 False False,
            Player "Jonathan" [Card Two Hearts, Card Five Clubs] 500 0 False False,
            Player "Lewis" [Card Nine Hearts, Card King Clubs] 500 0 False False]

table3 :: Table
table3 = Table 
            { players = players3,
              deck = fullDeck,
              board = [Card Five Hearts, 
                       Card Three Clubs, 
                       Card Four Diamonds, 
                       Card Six Clubs, 
                       Card Nine Diamonds],
              phase = Showdown,
              highBet = 0,
              pot = 600,
              dealerPosition = 3,
              smallBlindPosition = 0,
              bigBlindPosition = 1,
              bets = []
            }