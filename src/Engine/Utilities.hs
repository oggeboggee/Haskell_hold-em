module Engine.Utilities where


import Engine.EngineTypes

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
    let n = length playersList
        nextPlayer = (i + 1) `mod` n
        player = playersList !! nextPlayer
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
--   Multi-player:
--     PreFlop: UTG (one left of BB) acts first.
--     Postflop: first active player left of the dealer acts first.
firstPlayerToBet :: Table -> PlayerIndex
firstPlayerToBet table =
    let playersList = players table
        n = length playersList

        startIdx 
            | n == 2 && phase table == PreFlop = dealerPosition table                                   -- In heads up preflop, dealer acts first
            | n == 2                            = bigBlindPosition table                                -- In heads up postflop, BB acts first
            | phase table == PreFlop           = nextPlayerToAct (bigBlindPosition table) playersList   -- In preflop woth 3+ players, action starts to the left of the BB
            | otherwise                        = nextPlayerToAct (dealerPosition table) playersList     -- In postflop with 3+ players, action starts to the left of the dealer
        
        indices = [(startIdx + k) `mod` n | k <- [0..n-1]]      -- generate player indices in circular order starting from the starting index.

        canAct i = 
            let p = playersList !! i                            -- get player at index i
            in not (folded p) && chips p > 0 && not (acted p)   -- player must be not folded, have chips, and not already acted.

        valid = filter canAct indices       -- Keep only the players who are allowed to act.

    in case valid of
        (i:_) -> i          -- return first valid player
        []    -> startIdx   -- No valid player found, return the starting index.


-- | We need a function to take a player at a specific index in a list, 
--   and then replace with the updated player
replacePlayer :: PlayerIndex -> Player -> [Player] -> [Player]
replacePlayer playerIndex updatedPlayer playerList = 
    take playerIndex playerList ++ [updatedPlayer] ++ drop (playerIndex + 1) playerList


-- | Updates a player after showdown, adds their share of pot if they are winner. if player
--   is not in the list of winners then they don't get a share.
dealOutChips2 :: [PlayerName] -> Int -> Player -> Player
dealOutChips2 winners' share p
    | name p `elem` winners' = p { chips = chips p + share }
    | otherwise        = p



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

---------- Checkers to check different part of the state -----------------------

-- | Returns True when the current betting round is finished.
--   A round is over when every active player has either:
--   No chips left (all-in) or has matched the current high bet and acted.
--   The 'acted' flag is important because without it the BB would be considered
--   done after placing the BB, since their commitedchips already equals the high bet.
bettingRoundOver :: Table -> Bool
bettingRoundOver table = 
    let active = activePlayers table
        hb     = highBet table
        allMatched =
            all (\p ->
                chips p <= 0 ||
                (commitedChips p == hb && acted p)
            ) active

    in length active <= 1 || allMatched

----------------------------------------------------------------
-- Utility functions to control flow of 'runPhase' and bettinglogic

-- | Helper to extract active players still in the hand (i.e not folded)
activePlayers :: Table -> [Player]
activePlayers table =
    filter (not . folded) (players table)

-- | Determine if a betting round can start. Can only happen if
--   at least two players still have chips
bettingRoundCanRun :: Table -> Bool
bettingRoundCanRun table =
    let canAct = filter (\p -> chips p > 0) (activePlayers table)
    in length canAct > 1

-- | Check if hand is over. This is true when only one or zero players remain active (everyone else folded).
--   Does not consider the special case of everybody being all-in.
handOver :: Table -> Bool
handOver table =
    length (activePlayers table) <= 1

-- | Special case, for when no more betting can happen and the board should run out.
allPlayersAllIn :: Table -> Bool
allPlayersAllIn table =
    let active = activePlayers table
    in not (null active) && all (\p -> chips p == 0) active



----------------------------------------------------------------
-- | Pure function, decrease the amount of chips a player have by a certain amount
decChips :: Player -> Bet -> Player
decChips player bet = player {chips         = chips player - bet,
                              commitedChips = commitedChips player + bet}

-- | increase the amount of chips a player have by a certain amount
incChips :: Player -> Pot -> Player
incChips player pot' = player {chips = chips player + pot'}

-- | Reset the commited chips a player have made to zero
resetCommited :: Player -> Player
resetCommited player = player {commitedChips = 0}

-- | Increase the Pot by a certain amount
incPot :: Pot -> Bet -> Pot
incPot pot' bet = pot' + bet

-- | Helper function to calculate lowest bet a player can make
lowestBet :: Table -> Player -> Bet
lowestBet table player = highBet table - commitedChips player



                            