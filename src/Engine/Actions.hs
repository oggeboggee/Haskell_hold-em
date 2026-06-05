{-|
Module      : Engine.Actions
Description : Applying events and handle player actions

This module handle events, progressing the game state when players make an actions,
when a hand is over or a player is eliminate. The two main function is applyEvent and runShodown

Summerised responsibilities:

* Checking if a player action is valid.
* Applying valid player actions.
* Running showdown, determening the winner, after last betting round.
* Removing eliminated players(players with no chips left) from the table. 

The module also include some pure helper functions to help change the state correctly.
-}

{-# LANGUAGE TupleSections #-}

module Engine.Actions where

import Engine.EngineTypes
import Engine.Utilities
import Engine.HandEvaluation

import Control.Monad.State


-- | Entry point for state chaning events. Takes an Event (PlayerEvent or EngineEvent) and applies it to the current table state.
-- | Player actions are validated before being applied. Engineactions are applied directly if they cant fail (i.e PlaceBlind).
-- | Returns either an error message (Left) or a list of GameEvents (Right) describing what happened as a result of the action.
applyEvent :: Event -> State Table (Either String [GameEvent])

-- PLAYERACTIONS: Validates that the chosen action is legal given the current tables state then
-- applies it and returns the resulting GameEvent to be broadcast to clients.
applyEvent (PlayerEvent playerIndex action) = do
    t <- get
    let player = players t !! playerIndex
        hb     = highBet t
        cChips = commitedChips player
        plName = name player
        toCall = hb - cChips        -- What the player needs to put in to call the current high bet.

    case action of
        -- Fold: a player gives up their hand and is out of the current hand. Player is marked as
        -- folded and excluded from future betting rounds and the showdown until the next hand starts.
        Fold -> do
            modify (`pureFold` playerIndex)
            pure (Right [PlayerFolded plName])

        -- Check: only valid when there is no bet to match (toCall == 0).
        -- If there is a bet to match, the player must either call, raise, or go all in.
        Check -> do
            if toCall == 0
            then do
                modify (`pureCheck` playerIndex)
                pure (Right [PlayerChecked plName])
            else
                pure (Left "You can't check when behind the highbet.")

        -- Call: only valid when there is a bet to match (toCall > 0).
        Call -> do
            if toCall <= 0 || hb > chips player
            then pure (Left "There isn't a bet to call or you don't have enough chips to call.")
            else do

                let amount = hb - cChips
                modify (\t' -> placePureBet t' playerIndex amount)
                playerHaveActed playerIndex -- Change a players acted to True
                pure (Right [PlayerCalled plName amount])

        -- Raise: the raise amount is the amount of chips the player wants to add on top of calling the current high bet.
        -- Rejected if x is zero, negative, or larger than the amount of chips the player has available.
        Raise x -> do
            if x <= 0 || x > (chips player - lowestBet t player)
            then pure (Left "Raise amount must be larger than 0 and smaller then the amount of chips you have")
            else do

                let callAmount = hb - cChips
                    totalAmount = callAmount + x
                modify (\t' -> placePureBet t' playerIndex totalAmount)
                playerHaveActed playerIndex

                pure (Right [PlayerRaised plName x])

        -- AllIn: player commits all their remaining chips to the pot.
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


-- | Evaluates all active hands against the board and awards the pot to the winner(s).
--   Eliminates any players who have been put to zero chips. In case of a tie the pot is split evenly among the winners.
--   Returns ShowdownHappened, ChipsAwarded and PlayersEliminated events for broadcasting.
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

        updatedPlayers = map (dealOutChips winnerNames evenShare) playerList

        -- Bug fix name p `notElem` winnerNames so we never filter out the winner of a hand
        eliminatedNames = map name (filter (\p -> (chips p == 0) && (name p `notElem` winnerNames)) updatedPlayers)

    modify (\t -> t 
        { players = filter (\p -> name p `notElem` eliminatedNames) updatedPlayers
        , pot = 0 
        })
    
    pure [ ShowdownHappened winnerNames
         , ChipsAwarded (map (, evenShare) winnerNames)
         , PlayerEliminated eliminatedNames
         ]


------------------------------------------------------------------------------------
-- | Helper that applies a transformation function to a player at a given index and returning
--   the updated table. Used for actions like fold, check, call and raise.
updatePlayerAtIndex :: PlayerIndex -> (Player -> Player) -> Table -> Table
updatePlayerAtIndex playerIndex f table =
    if playerIndex < 0 || playerIndex > (length (players table) - 1)
        then error "Invalid index in updatePlayerAtIndex"
    else
        let playerList = players table
            player = playerList !! playerIndex
            updatedPlayer = f player
            updatePlayerList = replacePlayer playerIndex updatedPlayer playerList
        in table { players = updatePlayerList }

-- | Decrements a players chips, adds it to the pot, and updates the high bet if the players
--   total commited chips exceeds the current high bet. Used for call and raise actions.
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

-- | Marks a player as folded by setting their folded flag to True. 
--   They will be excluded from future betting rounds and the showdown until the next hand starts.
pureFold :: Table -> PlayerIndex -> Table
pureFold table playerIndex =
    updatePlayerAtIndex playerIndex (\p -> p { folded = True }) table

-- | Marks a player as checked by setting their acted flag to True.
--   This is used to indicate that the player has taken their action for the current betting round.
pureCheck :: Table -> PlayerIndex -> Table
pureCheck table playerIndex =
    updatePlayerAtIndex playerIndex (\p -> p { acted = True }) table

-- | Marks a player as having acted this betting round.
--   Called after placePureBet for Call, Raise, and AllIn so that during the betting round
--   we can check if a player has already acted or not.
playerHaveActed :: Int -> State Table ()
playerHaveActed playerPos = do
    modify (updatePlayerAtIndex playerPos (\p -> p {acted = True}))


