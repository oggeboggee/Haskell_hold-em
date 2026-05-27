module Engine.Utilities where

import Engine.EngineTypes


--------------------------------------------------------------------------------------------------------
-- PHASE
--------------------------------------------------------------------------------------------------------

-- | Advance a GamePhase to the next in the sequence.
nextPhase :: GamePhase -> GamePhase
nextPhase p = case p of
    DealHands   -> PreFlop
    PreFlop     -> Flop
    Flop        -> Turn
    Turn        -> River
    River       -> Showdown
    Showdown    -> DealHands

--------------------------------------------------------------------------------------------------------
-- TURN ORDERING
--------------------------------------------------------------------------------------------------------

-- | Finds the next player in turn order who is eligible to act. I.e not folded and still has chips.
-- | It wraps around the table.
nextPlayerToAct :: PlayerIndex -> [Player] -> PlayerIndex
nextPlayerToAct i playersList = 
    let n          = length playersList
        nextPlayer = (i + 1) `mod` n
        player     = playersList !! nextPlayer
    in if folded player || chips player == 0
       then nextPlayerToAct nextPlayer playersList
       else nextPlayer 


-- | Returns the index of the next player who still needs to act this betting round.
--   Starts from the position that acts first for the current phase, then walks forward
--   until it finds a player who is not folded, has chips, and hasn't acted yet.
--
--   Heads-up special cases (2 players):
--     PreFlop: dealer/SB acts first (posts blind, then acts before BB).
--     Postflop: BB acts first (positional advantage switches postflop).
--
--   3+ players:
--     PreFlop: UTG (one left of BB) acts first.
--     Postflop: first active player left of the dealer acts first.

-- Falls back to the startIdx if no valid player is found (i.e all players are all-in)
firstPlayerToBet :: Table -> PlayerIndex
firstPlayerToBet table =
    let playersList = players table
        n           = length playersList

        startIdx 
            | n == 2 && phase table == PreFlop = dealerPosition table                                   -- In heads up preflop, dealer acts first
            | n == 2                           = bigBlindPosition table                                 -- In heads up postflop, BB acts first
            | phase table == PreFlop           = nextPlayerToAct (bigBlindPosition table) playersList   -- In preflop woth 3+ players, action starts to the left of the BB
            | otherwise                        = nextPlayerToAct (dealerPosition table) playersList     -- In postflop with 3+ players, action starts to the left of the dealer
        
        -- All indices in circular order starting from startIdx
        indices = [(startIdx + k) `mod` n | k <- [0..n-1]] 

        canAct i = 
            let p = playersList !! i                            -- get player at index i
            in not (folded p) && chips p > 0 && not (acted p)   -- player must be not folded, have chips, and not already acted.

        -- Keep only the players who are allowed to act.
        valid = filter canAct indices

    in case valid of
        (i:_) -> i          -- return first valid player
        []    -> startIdx   -- No valid player found, return the starting index.


--------------------------------------------------------------------------------------------------------
-- HELPERS FOR PLAYER LIST
--------------------------------------------------------------------------------------------------------

-- | Replaces the player at a given index with an updated player. Used by all
--   functions that modify a players state.
replacePlayer :: PlayerIndex -> Player -> [Player] -> [Player]
replacePlayer playerIndex updatedPlayer playerList = 
    take playerIndex playerList ++ [updatedPlayer] ++ drop (playerIndex + 1) playerList


-- | Awards the winner(s) their share of the pot. Players who havent
--   won are returned in an unchanged state
dealOutChips :: [PlayerName] -> Int -> Player -> Player
dealOutChips winners' share p
    | name p `elem` winners' = p { chips = chips p + share }
    | otherwise              = p


--------------------------------------------------------------------------------------------------------
-- HELPERS TO ADD/REMOVE PLAYERS
--------------------------------------------------------------------------------------------------------

-- | Removes a player from the table by name.
--   Gets called when a player leaves or is eliminated.
removePlayer :: PlayerName -> Table -> Table
removePlayer n table =
    table { players = filter (\p -> name p /= n) (players table)}

-- | Seates a new player at the table with a starting stack of 1000 chips. The 
--   player is appended to the end of the players list.
addPlayer :: PlayerName -> Table -> Table
addPlayer n table =
    table { players = players table ++ [newPlayer] }
    where
        newPlayer = Player 
            { name = n
            , hand = []
            , chips = 1000
            , commitedChips = 0
            , folded = False
            , acted = False
            }

--------------------------------------------------------------------------------------------------------
-- CHECKS FOR BETTING STATE
--------------------------------------------------------------------------------------------------------

-- | Returns True when the current betting round is finished.
--   A round is over when every active player has either gone all in or matched the high bet and acted.
--   The acted flag here helps here because without it the BB player would appear as acted = True
--   on the first round as their commited chips would already equal the high bet in the case where
--   all other players call the BB. 
bettingRoundOver :: Table -> Bool
bettingRoundOver table = 
    let active = activePlayers table
        hb     = highBet table
        allMatched =
            all (\p -> chips p <= 0 || (commitedChips p == hb && acted p)) active
    in length active <= 1 || allMatched

-- | Check if hand is over. This is true when only one or zero players remain active (everyone else folded).
--   Does not consider the special case of everybody being all-in.
handOver :: Table -> Bool
handOver table =
    length (activePlayers table) <= 1

-- | Returns True when at least two players are active and a bettign round can procees.
--   False if there are less than two players who still have chips.
bettingRoundCanRun :: Table -> Bool
bettingRoundCanRun table =
    let canAct = filter (\p -> chips p > 0) (activePlayers table)
    in length canAct > 1

-- | Returns True when all active players are all in and no more betting can happen.
--   USed to detect when the board should be run out automatically.
allPlayersAllIn :: Table -> Bool
allPlayersAllIn table =
    let active = activePlayers table
    in not (null active) && all (\p -> chips p == 0) active

-- | Helper to extract active players still in the hand (i.e not folded)
activePlayers :: Table -> [Player]
activePlayers table =
    filter (not . folded) (players table)


--------------------------------------------------------------------------------------------------------
-- CHIP CALCULATIONS
--------------------------------------------------------------------------------------------------------

-- | Deducts a bet from the players chips and adds them to their commited chips total
decChips :: Player -> Bet -> Player
decChips player bet = player {chips         = chips player - bet,
                              commitedChips = commitedChips player + bet}

-- | Awards chips to a player.
incChips :: Player -> Pot -> Player
incChips player currentPot = player {chips = chips player + currentPot}


-- | Resets a players commited chips to zero.
resetCommited :: Player -> Player
resetCommited player = player {commitedChips = 0}

-- | Increase the Pot by a bet amount.
incPot :: Pot -> Bet -> Pot
incPot currentPot bet = currentPot + bet


-- | The minimum amount a player must put in to match the current high bet.
lowestBet :: Table -> Player -> Bet
lowestBet table player = highBet table - commitedChips player                     
