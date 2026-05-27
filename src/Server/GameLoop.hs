module Server.GameLoop 
    ( handleTransition
    , makeSnapshot
    , makePublicSnapshot
    ) where

import Server.ServerTypes
import Server.ServerHelpers
import Server.Protocol

import Engine.EngineTypes
import Engine.Actions       (applyEvent)
import Engine.TexasEngine   (removePlayer, startHand, stepGame)
import Engine.Utilities     (firstPlayerToBet, bettingRoundOver, activePlayers)

import qualified Data.Map.Strict as M

import Control.Monad.State       (runState)
import System.Random             (newStdGen)



------------------------------------------------------------------------------------------
-- STATE TRANSITION HANDLER
--
-- handleTransition is the single entry point for all state changes.
-- Returns a new ServerState and a list of BroadcastActions to be executed by NetworkServer.
-- GameLoop handles state logic only; NetworkServer executes the IO actions.
------------------------------------------------------------------------------------------

handleTransition :: ServerState -> Transition -> IO (ServerState, [BroadcastAction])

----------------
-- PLAYER JOINS
-- 
-- A player wants to join the table. Reject duplicate names, then update client record and add them
-- to the engines player list ('addPlayer'). The client/player who joins receives a full (private) snapshot
-- and the other clients get a JoinedTable message and a public snapshot update.
----------------
handleTransition st (PlayerJoined cid n) 

    -- Reject if the name is already in use by another connected client, notify only the joining client.
    | clientExists n st = do
        pure (st, [SendToClient cid (ErrorMsg "Name already taken")])
    
    | otherwise = do
        let st1 = nameClient cid n st           -- Assign the chosen name to this ClientId in the clients map.
            st2 = addPlayerToLobby n st1        -- Append player to the back of the FIFO lobby queue.
            
            actions = 
                [ SendToClient cid (LobbyJoined n (length (lobby st2))) -- Tell joining client their lobby position.
                , BroadcastToAll (LobbyUpdate (lobby st2))              -- Tell all clients the lobby queue changed.
                ]
        
        -- After every lobby change check if there is a hand currently playing and if there are enough players to start a hand.
        if handOngoing st then pure (st2, actions)              -- If a hand is playing we only add a player to the lobby
        else do
            (finalSt, checkActions) <- checkLobbyAndStart st2   -- Else we seat players in the lobby at the table

            pure (finalSt, actions ++ checkActions)
            
------------------
-- PLAYER LEAVES
--
-- A player leaves the table. Removes them from both the client map and the engines player list.
-- Broadcasts to the other clients that the client left. This is only for clients who
-- explicitly leave, not disconnects.
------------------
handleTransition st (PlayerLeft cid) =
    case lookupPlayer cid st of
        -- Client connected but never sent JoinMsg, so they haven't got a name
        -- and aren't in the lobby or at the table yet.
        Nothing ->
            pure (st, [])
        
        Just plName -> do
            let st1 = removeClient cid st                             -- Remove from WS clients map.
                st2 = removePlayerFromLobby plName st1                -- Remove form lobby queue if waiting.
                st3 = st2 { table = removePlayer plName (table st2) } -- Remove from table if seated.
                
                actions =
                    [ BroadcastToAll (LeftTable plName)               -- Tell all clients this player left.
                    , BroadcastToAll (makeSnapshot (table st3))       -- Send updated table state.
                    , BroadcastToAll (LobbyUpdate (lobby st3))        -- Send updated lobby queue.
                    ]

            pure (st3, actions) 

------------------
-- Player Action
--
-- A player takes an action in the game. 
-- First we resolve their ClientId to a playerindex in the table that the engine understands.
-- Then run the action through the engines 'applyEvent' function, and after broadcast the results to all clients.
--
------------------
handleTransition st (PlayerActed cid action) =
    case resolvePIdx st cid of
        Nothing -> do
            -- The client isn't a seated player, reject action.
            -- Safety measure incase the clients map and players list in engine aren't in sync. 
            -- (A player can be connected and not yet have a name, this means they wont be in the engines players list.)
            pure (st, [SendToClient cid (ErrorMsg "Not seated at table")])

        Just pIdx -> do
            -- Enforce turn order by checking if the acting player is the one the engine expects to act next.
            -- firstPlayerToBet returns the index of the next player who hasn't acted yet.
            let expectedIdx = firstPlayerToBet (table st)
            if pIdx /= expectedIdx
                then do
                    -- Not this player's turn, notify client of rejected action.
                    pure (st, [SendToClient cid (ErrorMsg "Not your turn")])
                else do
                    -- Action is from the correct player, pass to engine as a PlayerEvent.
                    -- 'applyEvent' is a pure state functino. We run it against the current table and get
                    -- either an error or a list of GameEvents, along with the updated table state.
                    let event = PlayerEvent pIdx action
                        (result, newTable) = runState (applyEvent event) (table st)
                            
                    case result of
                        -- The engine rejected the action. 
                        -- Tell only the player trying to make the action, and no state change.
                        Left invalid -> do
                            pure (st, [SendToClient cid (ErrorMsg invalid)])

                        -- The action was valid. Broadcast to clients what happened (the GameEvents) and
                        -- then update the clients with the new table state using a snapshot.
                        Right gameEvents -> do
                            let newState = st { table = newTable }
                            (finalSt, stepActions) <- handleGameStep newState
                    
                            let initialActions =
                                    [ BroadcastToAll (GameEventMsgs gameEvents)   -- What just happened, e.g. PlayerCalled, PlayerFolded, PlayerRaised etc.
                                    , BroadcastToAll (makeSnapshot newTable)      -- Updated snapshot with new table state after said actions.
                                    ]
                    
                            pure (finalSt, initialActions ++ stepActions)

------------------------------------------------------------------------------------------
-- GAME PROGRESSION
-- 
-- | Called after every successful player action.
--   Asks the engine what state the game is in and responds accordingly.
--   Recurses until the engine signals AwaitingAction (waiting for a player)
--   or HandComplete (hand is over and the showdown has run).
------------------------------------------------------------------------------------------
handleGameStep :: ServerState -> IO (ServerState, [BroadcastAction])
handleGameStep st = do
    case stepGame (table st) of

        -- Betting round is still ongoing, nothing to advance. Just wait for the next player action.
        AwaitingAction -> 
            -- Tell the next player to act that it's their turn, along with the actions they can take and the amount to call
            let turnActions = yourTurnMessage st
            in pure (st, turnActions)

        -- The bettinground ended and the engine advanced to the next phase.
        -- Broadcast to all clients what happened during the change (cards being delt, phase change etc.)
        -- Then recurse to check if the next phase also needs to advance (e.g if all players went all in).
        PhaseAdvanced events newTable -> do
            let newSt = st { table = newTable }
                actions = if null events
                          then []
                          else [BroadcastToAll (GameEventMsgs events)]

            -- Send the snapshot before recursing so clients see the new phase and cards on the board.
                initialActions = actions ++ [BroadcastToAll (makeSnapshot (table newSt))]
            
            (finalSt, nextActions) <- handleGameStep newSt
            pure (finalSt, initialActions ++ nextActions)

        -- The hand is finished, the engine ran the showdown, awarded the chips, and eliminated
        -- eliminated players.
        HandComplete events newTable -> do
            let newSt = st { table = newTable, handOngoing = False }
                -- Remove eliminated players from the lobby so they can't rejoin.
                eliminatedNames = map name (filter (\p -> chips p == 0) (players newTable))
                cleanedSt = newSt { lobby = filter (`notElem` eliminatedNames) (lobby newSt) } -- Add handOngoing change
                actions = [ BroadcastToAll (GameEventMsgs events)                       -- Showdown result and chip awards.
                                , BroadcastToAll (makeSnapshot (table cleanedSt))       -- Final table state after showdown
                                , createShowdownHands newSt                             -- Reveal everyone's hands at shwodown.
                                ]

            -- After a hand ends, see if conditions are met to start a new one.
            (finalSt, checkActions) <- checkLobbyAndStart newSt
            if length (activePlayers (table st)) > 1 then
                pure (finalSt, actions ++ checkActions)
            else 
                pure (finalSt, checkActions)

------------------------------------------------------------------------------------------
-- LOBBY AND HAND START
--
-- checkLobbyAndStart is called whenever the lobby changes and decided if conditions
-- are met to start a hand. 
-- Moves players to lobby first to get a clean count and then decides.
------------------------------------------------------------------------------------------

checkLobbyAndStart :: ServerState -> IO (ServerState, [BroadcastAction]) -- Add check state of handOngoning 
checkLobbyAndStart st =
    let numPlayers = length (lobby st) + length (players (table st))  -- Kolla kombinationen antal spelare vid bord och lobby

    in if numPlayers < 2
        then pure (st,
            [ BroadcastToAll (makeSnapshot (table st))
            , BroadcastToAll (LobbyUpdate (lobby st))
            ])
        else tryToStartHand st


-- | Seat the players from the lobby and try to start a new hand
--   Called once checkLobbyAndStart has confirmed there are enough players.
--
-- 1. 'returnPlayersToLobby': Move all seated players back to the front of the lobby
-- 2. 'seatLobbyPlayersAtTable': Fill the seats from the front of the lobby queue.
-- 3. Run the engines 'startHand' with a 'newStdGen'
-- 4. Return startup events, updated snapshot, private hands, and the first Yourturn message.

tryToStartHand :: ServerState -> IO (ServerState, [BroadcastAction])
tryToStartHand st = do
    -- st already has empty table and players in lobby from checkLobbyAndStart

    let 
        st2    = seatLobbyPlayersAtTable st    -- Seat players up to the max seat limit.
        
        
        -- st1    = returnPlayersToLobby st        -- Move all players again to ensure table is cleared.
        -- st2    = seatLobbyPlayersAtTable st1    -- Seat players up to the max seat limit.

        seated = numSeatedPlayers st2

    -- Safety check - should always pass given checkLobbyAndStart, but just in case.
    if seated < 2
        then pure (returnPlayersToLobby st, -- Used to st1
            [ BroadcastToAll (makeSnapshot (table st2))
            , BroadcastToAll (LobbyUpdate (lobby st2))
            ])
        else do
            -- Generate randomness here in IO, pass into the engine. Engine doesn't touch IO,
            -- it just uses the randomness we give it to start the hand.
            gen <- newStdGen
            let (startupEvents, newTable) = startHand gen (table st2)
                st3 = st2 { table = newTable, handOngoing = True }
                actions =
                    [ BroadcastToAll (GameEventMsgs startupEvents)  -- Broadcast blinds being placed.
                    , BroadcastToAll (makeSnapshot (table st3))     -- The table state after setup.
                    , BroadcastToAll (LobbyUpdate (lobby st3))      -- The updated lobby with seated players removed.
                    ] ++ createPrivateHandActions st3               -- Each player gets sent their hand privately.
                      ++ yourTurnMessage st3                        -- Tell the first player to act it's their turn.
            pure (st3, actions)


------------------------------------------------------------------------------------------------
-- PRIVATE HANDS
-- 
-- Builds a SendToClient action for each seated player which contains their private hands.
-- Any players in the lobby, unnamed clients, or disconnecting clients are skipped here.
-- Called once at the start of each hand so each playeronly sees their own cards.
------------------------------------------------------------------------------------------------

createPrivateHandActions :: ServerState -> [BroadcastAction]
createPrivateHandActions st =
    concatMap sendPrivateHand (M.toList (clients st))
    where
        sendPrivateHand (cid, info) =
            case findPlayerByName (clientName info) (table st) of
                -- Client not seated, either in lobby, unnamed, or disconnecting so skip
                Nothing -> [] 

                -- Send this player their hands privately.
                Just p -> [SendToClient cid (PrivateHand (map show (hand p)))] 

------------------------------------------------------------------------------------------------
-- YOUR TURN MESSAGE
-- 
-- Builds a YourTurn broadcast telling all clients whose turn it is and what actions they can take.
-- Returns empty list if no action is needed.
------------------------------------------------------------------------------------------------
yourTurnMessage :: ServerState -> [BroadcastAction]
yourTurnMessage st =
    let t = table st
    in if null (players t) || bettingRoundOver t
        then []
        else
            let idx    = firstPlayerToBet t     -- index of next player who must act.
                player = players t !! idx
                n      = name player
                hb     = highBet t
                toCall' = hb - commitedChips player -- How much the player needs to bet to stay in.

                -- The available actions depend on if there is a bet for the player to match or not.
                actions
                    | toCall' <= 0 = ["check", "fold", "raise", "allin"]    -- No bet to call, can check.
                    | otherwise   = ["call", "fold", "raise", "allin"]      -- Bet to call, may not check.

            in [BroadcastToAll (YourTurn n actions toCall' (chips player))]

------------------------------------------------------------------------------------------
-- SNAPSHOT BUILDER
--
--
-- Snapshot = complete view of current game state (Table), we send this to clients after every state change for UI to stay in sync.
-- 'makePublicSnapshot' and 'makeSnapshot' are identical right now. 
-- We need distinction later to have private visibility of hands, so we can separate the types of snapshots.
--
-- 'makeSnapshot' = for the joining player's private view. to show hands etc.
-- 'makePublicSnapshot' = for everyone else, with hidden hands.
------------------------------------------------------------------------------------------

-- | Build a snapshot in order to broadcast to clients. Gets sent to all existing players when someone joins.
makePublicSnapshot :: Table -> ServerMsg
makePublicSnapshot = makeSnapshot

-- | Builds a snapshot of the current game state (Table).
--   Hands are hidden for now. We can add later to provide private hand dealing.
makeSnapshot :: Table -> ServerMsg
makeSnapshot t = Snapshot
    { snapPlayers = map toPlayerSnap (players t)
    , snapPot     = pot t
    , snapPhase   = show (phase t)
    , snapBoard   = map show (board t)
    , snapHighBet = highBet t
    , snapDealer  = getNameOfDealer t
    , snapPlayerTurn = getTurnPlayer t
    }
    where
        -- | Convert a player in the engine to a PlayerSnapshot that is safe to broadcast.
        --   snapHand hidden, should not be broadcasted publicly.
        toPlayerSnap p = PlayerSnapshot
            { snapName = name p
            , snapChips = chips p
            , snapCommited = commitedChips p
            , snapFolded   = folded p
            , snapHand     = Nothing    -- Never revelaed in the public snapshot.
            }

-- | Returns the name of the player currently holding the dealer button.
--  If no players at the table, returns an empty string.
getNameOfDealer :: Table -> PlayerName
getNameOfDealer t =
    if null (players t)
        then ""
        else name (players t !! dealerPosition t)

-- | Returns the name of the player whose turn it is to act.
getTurnPlayer :: Table -> PlayerName
getTurnPlayer t
    | null (players t) = ""
    | bettingRoundOver t = ""
    | otherwise = name (players t !! firstPlayerToBet t)


-- | Reveals all players hands to all clients at showdown.
--   Called once per hand after HAndComplete, after chips have been awarded.
createShowdownHands :: ServerState -> BroadcastAction
createShowdownHands st = 
    let playerNotFolded = activePlayers (table st)
        hands = map (\p -> (name p, map show (hand p))) playerNotFolded
    in BroadcastToAll (ShowdownHands hands)