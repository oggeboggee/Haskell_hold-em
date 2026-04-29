module Utilities where


import Types
import Cards
--import Actions
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

-- Axel
{-
-- | Choose who is the next player to act, return the index of that player
nextPlayerToAct :: Int -> [Player] -> Int
nextPlayerToAct i players = if not (hasFolded (players!!next)) then next
                            else nextPlayerToAct (i+1) players
                                where
                                    next = (i + 1) `mod` (length players)
-}
 -- | Finds the next player in turn order who is eligible to act. I.e not folded and still has chips.
-- | It wraps around the table.
nextPlayerToAct :: PlayerIndex -> [Player] -> PlayerIndex
nextPlayerToAct i playersList = 
    let n = (length playersList)
        nextPlayer = (i + 1) `mod` n
        player = playersList !! nextPlayer
    in if ((folded player) || (chips player) == 0)
       then nextPlayerToAct nextPlayer playersList
       else nextPlayer 

-- Jonathan
-- | Who bets first varies depedning on if we are in PreFlop state or any other state. PreFlop it is
--   the player UTG (BB+1). In all postflop betting rounds action begins with the player in the SB position.
firstPlayerToBet :: Table -> PlayerIndex
firstPlayerToBet table = case phase table of
    PreFlop -> nextPlayerToAct (bigBlindPosition table) (players table)
    _       -> (smallBlindPosition table)


-- | We need a function to take a player at a specific index in a list, 
--   and then replace with the updated player
replacePlayer :: PlayerIndex -> Player -> [Player] -> [Player]
replacePlayer playerIndex updatedPlayer playerList = 
    take playerIndex playerList ++ [updatedPlayer] ++ drop (playerIndex + 1) playerList


filterFolded :: [Player] -> [Player]
filterFolded  = filter (not . hasFolded)

-- Axel
{-
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
-}
-- | Helper function to deal out the winning to the players
dealOutChips :: [Player] -> [Int] -> Int -> [Player]
dealOutChips players []      _     = players
dealOutChips players (x:xs)  chips = dealOutChips players' indexes' chips
    where
        indexes' = xs
        player   = incChips (players!!x) chips
        players' = replacePlayer x player players

-- | Updates a player after showdown, adds their share of pot if they are winner. if player
--   is not in the list of winners then they don't get a share.
dealOutChips2 :: [PlayerName] -> Int -> Player -> Player
dealOutChips2 winners share p
    | name p `elem` winners = p { chips = chips p + share }
    | otherwise        = p

{-

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

---------- Checkers to check different part of the state -----------------------

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
roundOver2 players highBet = (and [(matchHBet p highBet && hasActed p) || hasFolded p | p <- players]) 
-}
-- | Determines wether the current betting round is over or not. A round is over once eveery player has
-- | either folded, is all-in, or matched the current highBet and taken an action.
bettingRoundOver :: Table -> Bool
bettingRoundOver table = 
    let active = filter (not . folded) (players table)
        hb         = (highBet table)

        allMatched =
            all (\p ->
                folded p ||

                chips p <= 0 ||
                (commitedChips p == hb && acted p)
            ) active

    
    in length active <= 1 || allMatched
{-
-- | Check if a player have macthed the highest bet
matchHBet :: Player -> Int -> Bool
matchHBet player highBet = commitedChips player == highBet
-}
-- | Check if player have folded
hasFolded :: Player -> Bool
hasFolded = folded 
{-
-- | Check if all players have acted
allHaveActed :: [Player] -> Bool
allHaveActed players = and [hasActed player | player <- players]    
-}

-- From Actions
    
-- | Pure function, decrease the amount of chips a player have by a certain amount
decChips :: Player -> Bet -> Player
decChips player bet = player {chips         = (chips player)-bet,
                              commitedChips = (commitedChips player)+bet}

-- | increase the amount of chips a player have by a certain amount
incChips :: Player -> Pot -> Player
incChips player pot = player {chips = (chips player)+pot}

-- | Reset the commited chips a player have made to zero
resetCommited :: Player -> Player
resetCommited player = player {commitedChips = 0}

-- | Increase the Pot by a certain amount
incPot :: Pot -> Bet -> Pot
incPot pot bet = pot + bet

-- | Helper function to calculate lowest bet a player can make
lowestBet :: Table -> Player -> Bet
lowestBet table player = highBet table - commitedChips player



                            