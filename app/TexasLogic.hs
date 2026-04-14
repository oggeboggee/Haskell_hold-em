module App.TexasLogic where

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
