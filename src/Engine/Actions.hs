{-# LANGUAGE TupleSections #-}

module Engine.Actions where

import Engine.EngineTypes
import Engine.Utilities
import Engine.HandEvaluation

import Control.Monad.State


-- | Here we update the state, have betting rules (when can a player call, raise etc.). 
-- | We handle errors if wanted action isn't allowed. We produce GameEvents.
-- | This is instead of having applyPureAction.
applyEvent :: Event -> State Table (Either String [GameEvent])

-- PLAYERACTIONS
applyEvent (PlayerEvent playerIndex action) = do
    t <- get
    let player = players t !! playerIndex
        hb     = highBet t
        cChips = commitedChips player
        plName = name player
        toCall = hb - cChips

    case action of
        Fold -> do
            modify (`pureFold` playerIndex)
            pure (Right [PlayerFolded plName])

        Check -> do
            if toCall == 0
            then do
                modify (`pureCheck` playerIndex)
                pure (Right [PlayerChecked plName])
            else
                pure (Left "You can't check when behind the highbet.")

        Call -> do
            if toCall <= 0
            then pure (Left "There isn't a bet to call.")
            else do

                let amount = hb - cChips
                modify (\t' -> placePureBet t' playerIndex amount)
                playerHaveActed playerIndex -- Change a players acted to True
                pure (Right [PlayerCalled plName amount])

                -- modify (\t -> placePureBet t playerIndex toCall)
                -- pure (Right [PlayerCalled plName toCall])


        Raise x -> do
            if x <= 0 || x > (chips player + lowestBet t player)
            then pure (Left "Raise amount must be larger than 0 and smaller then the amount of chips you have")
            else do

                let callAmount = hb - cChips
                    totalAmount = callAmount + x
                modify (\t' -> placePureBet t' playerIndex totalAmount)
                playerHaveActed playerIndex

                pure (Right [PlayerRaised plName x])

        AllIn -> do
            let amount = chips player

            modify (\t' -> placePureBet t' playerIndex amount)
            playerHaveActed playerIndex -- Change a players acted to True

            pure (Right [PlayerAllIn plName amount])

-- System Events
applyEvent (EngineEvent engineAction) = do
    t <- get
    case engineAction of
        PlaceBlind playerIndex blindType bet -> do
            let player = players t !! playerIndex
                plName = name player
            modify (\t' -> placePureBet t' playerIndex bet)
            pure (Right [PlayerPlacedBlinds plName blindType bet])


        RunShowdown -> do
            Right <$> runShowdown

        EliminatePlayers -> do
            let eliminated = filter (\p -> chips p == 0) (players t)
                names      = map name eliminated
            modify (\t' -> t' { players = filter (\p -> name p `notElem` names) (players t')})

            pure(Right [PlayerEliminated names]) 

runShowdown :: State Table [GameEvent]
runShowdown = do
    table <- get

    let playerList = players table
        finalBoard = board table

        -- only the players that are still in the hand
        -- ex. [Sam, Lewis]
        activePl = filter (not . folded) playerList

        -- ex. [[5H,8C], [7H,9H]]
        extractedHands = map hand activePl

        -- ex- [1]  , indexes relative to the activeplayers
        winnerIndexes = winners finalBoard extractedHands

        -- winners returns index of winner(s) in activePlayers list, need to map to who it is
        -- in activePlayers: ex. activePlayers !! 1 = Lewis
        computedWinners = map (activePl !!) winnerIndexes

        potSize = pot table

        -- split the pot evenly
        evenShare =
            if null computedWinners
            then 0
            else potSize `div` length computedWinners

        winnerNames = map name computedWinners

        updatedPlayers = map (dealOutChips2 winnerNames evenShare) playerList

        eliminatedNames = map name (filter (\p -> chips p == 0) updatedPlayers)

    modify (\t -> t 
        { players = filter (\p -> name p `notElem` eliminatedNames) updatedPlayers
        , pot = 0 
        })
    
    pure [ ShowdownHappened winnerNames
         , ChipsAwarded (map (, evenShare) winnerNames)
         , PlayerEliminated eliminatedNames
         ]



-- | We need a function to take a player at a specific index in a list, 
--   and then replace with the updated player
-- replacePlayer :: PlayerIndex -> Player -> [Player] -> [Player]
-- replacePlayer playerIndex updatedPlayer playerList = 
--     take playerIndex playerList ++ [updatedPlayer] ++ drop (playerIndex + 1) playerList
updatePlayerAtIndex :: PlayerIndex -> (Player -> Player) -> Table -> Table
updatePlayerAtIndex playerIndex f table =
    let playerList = players table
        player = playerList !! playerIndex
        updatedPlayer = f player
        updatePlayerList = replacePlayer playerIndex updatedPlayer playerList
    in table { players = updatePlayerList }


-- placeBet that works with Gaem() 
--placeBet :: Int -> Bet -> State Table ()
placePureBet :: Table -> PlayerIndex -> Bet -> Table
placePureBet table playerIndex bet =
    let tableUpdated = updatePlayerAtIndex playerIndex
                        (`decChips` bet) table

        player = players tableUpdated !! playerIndex
    in tableUpdated
            { pot = incPot (pot tableUpdated) bet,
              bets = bet : bets tableUpdated,
              highBet = max (highBet tableUpdated) (commitedChips player)
            }

pureFold :: Table -> PlayerIndex -> Table
pureFold table playerIndex =
    updatePlayerAtIndex playerIndex (\p -> p { folded = True }) table


pureCheck :: Table -> PlayerIndex -> Table
pureCheck table playerIndex =
    updatePlayerAtIndex playerIndex (\p -> p { acted = True }) table


-- Change a players acted status to True
playerHaveActed :: Int -> State Table ()
playerHaveActed playerPos = do
    modify (updatePlayerAtIndex playerPos (\p -> p {acted = True}))



--addPlayer :: PlayerName -> Table -> Table
--addPlayer name t = t { players = players t ++ [Player name [] 1000 0 False False]}

--------------------------------------------------------------
--------------------------------------------------------------
