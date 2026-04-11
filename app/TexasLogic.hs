
module TexasLogic where

import App.Cards

------------------------------
-- | Chips and the pot is represented as Int
type Chip = Int
type Pot = Int

-- | Represent if a player have big, samll or no blind
data Blind = 
    NoBlind
    | SmallBlind
    | BigBlind

-- | All phases of the game
data GamePhase = 
            Dealhands
            | PreFlop
            | Flop
            | Turn
            | River
            | Showdown
    deriving (Show)

-- | Data type for a player
data Player = Player
            {
            name :: String,
            hand :: Hand,
            chips :: Chip,
            folded :: Bool,
            blind :: Blind
            }

-- | The table represent the gamestate
data Table = Table
            {
                players :: [Player],
                deck :: Deck,
                phase :: GamePhase,
                pot :: Pot,
                dealPosition :: Int
            }

------------------------------
-- | Progress the phase to the next phase
nextPhase :: GamePhase -> GamePhase
nextPhase p = case p of
    Dealhands   -> PreFlop
    PreFlop     -> Flop
    Flop        -> Turn
    Turn        -> River
    River       -> Showdown
    Showdown    -> Dealhands




