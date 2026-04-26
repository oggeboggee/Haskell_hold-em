module Utilities where


import Types
import Cards
import Actions
import Control.Monad.State
import Data.Char (toLower)
import System.Random
import HandEvaluation


--------------------------------------------------------------------------------
----------------------- Strictly pure helper functions -------------------------

-- | Progress the phase to the next phase
nextPhase :: GamePhase -> GamePhase
nextPhase p = case p of
    DealHands   -> PreFlop
    PreFlop     -> Flop
    Flop        -> Turn
    Turn        -> River
    River       -> Showdown
    Showdown    -> DealHands


-- | Choose who is the next player to act, return the index of that player
nextPlayerToAct :: Int -> [Player] -> Int
nextPlayerToAct i players = if not (hasFolded (players!!next)) then next
                            else nextPlayerToAct (i+1) players
                                where
                                    next = (i + 1) `mod` (length players)


-- | Given the state of a table and a specific player(index) return a string with avalible actions
availableActions :: Table -> Int -> String
availableActions table playerPos = if canCheck table playerPos then "Check, " ++ actions
                         else "Call, " ++ actions
    where
        actions = "Fold, Raise, AllIn"

-- | Filter out all players that have folded
filterFolded :: [Player] -> [Player]
filterFolded  = filter (not . hasFolded)

-- | Print the names(strings) of the player in a list
printNames :: [String] -> String
printNames []     = ""
printNames (x:[]) = x
printNames (x:xs) = " " ++ x ++ ", " ++ printNames xs

-- | Helper function to deal out the winning to the players
dealOutChips :: [Player] -> [Int] -> Int -> [Player]
dealOutChips players []      _     = players
dealOutChips players (x:xs)  chips = dealOutChips players' indexes' chips
    where
        indexes' = xs
        player   = incChips (players!!x) chips
        players' = replacePlayer x player players

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

---------------------------- Checkers ------------------------------------------

-- | Check if a player can check, return a bool
canCheck :: Table -> Int -> Bool
canCheck table playerPos = highBet table == 0 
                        || (playerPos == bigBlindPosition table 
                            && commitedChips (players table!!playerPos) == highBet table
                            && phase table == PreFlop)

-- | Check if and return True if only one active player is left 
onePlayerLeft :: [Player] -> Bool
onePlayerLeft players = (==1) . length $ filter not [hasFolded player | player <- players]

-- | Decide if a betting round is over
roundOver2 :: [Player] -> Int -> Bool
roundOver2 players highBet = (and [(matchHBet p highBet && hasActed p)|| hasFolded p | p <- players]) 

-- | Check if a player have macthed the highest bet
matchHBet :: Player -> Int -> Bool
matchHBet player highBet = commitedChips player == highBet

-- | Check if player have folded
hasFolded :: Player -> Bool
hasFolded = folded 

-- | Check if all players have acted
allHaveActed :: [Player] -> Bool
allHaveActed players = and [hasActed player | player <- players]    



                            