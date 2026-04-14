module TexasLogic where

import Cards

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
            DealHands
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
            hand :: Hand
            --chips :: Chip,
            --folded :: Bool,
            --blind :: Blind
            } deriving (Show)

-- | The table represent the gamestate
data Table = Table
            {
                players :: [Player],
                deck :: Deck,
                board :: CommunityCard,
                phase :: GamePhase
                --pot :: Pot,
                --dealerPosition :: Int
                --smallBlindPosition :: Int,
                --bigBlindPosition :: Int
            } deriving (Show)

------------------------------
-- | Progress the phase to the next phase
nextPhase :: GamePhase -> GamePhase
nextPhase p = case p of
    DealHands   -> PreFlop
    PreFlop     -> Flop
    Flop        -> Turn
    Turn        -> River
    River       -> Showdown
    Showdown    -> DealHands

-- | Updating what happens during each phase (dealing cards, showing community cards, etc.) and moves phase to next phase
-- This is all hardcoded for now but will change later.
gameStep :: Table -> Table
gameStep table = case phase table of
    DealHands -> dealHandsStep table
        {- let (h1, deck1) = dealCards (deck table) 2
            (h2, deck2) = dealCards deck1 2

            [player1, player2] = players table

            player1' = player1 {hand = h1}
            player2' = player2 {hand = h2}
        in table { players = [player1', player2'],
                   deck = deck2,
                   phase = nextPhase (phase table)
               
                 } -}
    PreFlop -> table {
        phase = nextPhase (phase table)
    }
        -- First betting rounds, player can bet, fold, call, raise. (logic we don't have yet)
    Flop ->
        let (flop, newDeck ) = dealCards (deck table) 3
        in table { board = flop,
                   deck = newDeck,
                   phase = nextPhase (phase table)
                 }

    Turn ->
       let (turn, newDeck) = dealCards (deck table) 1
       in table { board = board table ++ turn,
                  deck = newDeck,
                  phase = nextPhase (phase table)
                }

    River ->
       let (river, newDeck) = dealCards (deck table) 1
       in table { board = board table ++ river,
                  deck = newDeck,
                  phase = nextPhase (phase table)
                }

    Showdown -> table -- Here we have more betting logic, showing hands to all players?


dealHands :: [Player] -> Deck -> ([Player], Deck)
dealHands [] deck = ([], deck) --basecase, when no more players left return the deck.
dealHands (p1:players) deck = 
    let (deltHand, deck1) = dealCards deck 2
        p1'           = p1 {hand = deltHand}
        (players', deck2)  = dealHands players deck1
    in (p1' : players', deck2)
    -- This is same as hardcoded but made now for not just 2 players but any and all players.

dealHandsStep :: Table -> Table
dealHandsStep table = let (players', deckAfterDeltHAnds) = dealHands (players table) (deck table)
    in
        table {
            players = players',
            deck = deckAfterDeltHAnds,
            phase = nextPhase (phase table)
        } 


-- Temporary gameloop as a starting point
gameLoop :: Table -> IO ()
gameLoop table = do
    print table
    let newTable = gameStep table
    let newTable' = gameStep newTable
    let newTable'' = gameStep newTable'
    print newTable''
 

              