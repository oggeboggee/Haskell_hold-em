module TexasLogic where

import Cards
import Types
------------------------------------------------------------
------------------------------------------------------------
-- | Progress the phase to the next phase
nextPhase :: GamePhase -> GamePhase
nextPhase p = case p of
    Dealhands   -> PreFlop
    PreFlop     -> Flop
    Flop        -> Turn
    Turn        -> River
    River       -> Showdown
    Showdown    -> Dealhands

------------------------------------------------------------
-- | Calculate the lowest bet a player can make 
--lowestBet :: Table -> Player -> bet

-- | Calculate the highest commitedBet among players
highBet :: Table -> Bet
highBet table = (highBet table)


-- | Change blind status to given blind
-- changeBlind :: Player -> Blind -> Player
-- changeBlind player blind = player {blind player = blind}


-- | Change who have the blinds !! DOES NOT WORK YET
-- progressBlind :: [Player] -> [Player]
-- progressBlind []     = []
-- progressBlind (x:xs) = changeBlind x : progressBlind xs

-- | When a new round start all players need to be reset
-- resetPlayers :: [Player] -> [Player]

-- | NOT DONE YET
-- resetTable :: Table -> Table
-- resetTable table = table {highBet = 0,
--                           pot = 0,
--                           players = progressBlind players
--                           }

{- 
-- | TODO:
    * Actions
        * Bet
        * Check
        * Rise
        * Call
        * Fold
    * Bet
        * Add to Pot
          
    Need a system for looking at who have made an action
        and who can still make an action
        Something to keep track of whos turn it is
        If someone raise, players who already made an 
            action need to make a new action
            
    Also need something that calculates the lowest bet someone can make
-}
