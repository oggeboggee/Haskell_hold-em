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


-- | Updates a player after showdown, adds their share of pot if they are winner. if player
--   is not in the list of winners then they don't get a share.
dealOutChips2 :: [PlayerName] -> Int -> Player -> Player
dealOutChips2 winners share p
    | name p `elem` winners = p { chips = chips p + share }
    | otherwise        = p



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

---------- Checkers to check different part of the state -----------------------

-- | Determines wether the current betting round is over or not. A round is over once eveery player has
-- | either folded, is all-in, or matched the current highBet and taken an action.
bettingRoundOver :: Table -> Bool
bettingRoundOver table = 
    let active = filter (not . folded) (players table)
        hb     = highBet table

        allMatched =
            all (\p ->
                folded p ||
                chips p <= 0 ||
                (commitedChips p == hb && acted p)
            ) active

    
    in length active <= 1 || allMatched



-- From Actions
----------------------------------------------------------------
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



                            