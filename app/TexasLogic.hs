
module App.TexasLogic where

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
    deriving (Show)

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
            name   :: String,
            hand   :: Hand,
            chips  :: Chip,
            folded :: Bool,
            blind  :: Blind
            }
    deriving (Show)

-- | The table represent the gamestate
data Table = Table
            {
                players      :: [Player],
                playerTurn   :: Int,
                deck         :: Deck,
                board        :: CommunityCard
                phase        :: GamePhase,
                pot          :: Pot,
                dealPosition :: Int
            }
    deriving (Show)

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

-- | Take a list with all players and a list of player 
    -- that have not yet acted, and return the next player
-- nextPlayer :: [Player] -> [Player] -> Player
-- nextPlayer allPlayers []     = allPlayers!!0
-- nextPlayer allPlayers (x:xs) = x


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
          
-}
