module TexasEngine where

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
-- | Change blind status to given blind
-- changeBlind :: Player -> Blind -> Player
-- changeBlind player blind = player {blind player = blind}


