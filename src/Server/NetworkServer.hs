{-# LANGUAGE OverloadedStrings #-}

module Server.NetworkServer where

import Types.GameTypes
import Engine.Actions     (applyEvent)
import Engine.TexasEngine (removePlayer, addPlayer)
import Server.Protocol

import qualified Network.WebSockets  as WS
import qualified Data.Aeson          as Aeson
--import qualified Data.Text         as T
--import Data.Text (Text)            
import Data.List                     (findIndex)
import Data.Map                      (Map)
import qualified Data.Map.Strict     as M
-- https://stackoverflow.com/questions/20576229/an-example-of-using-data-map-in-haskell

import Control.Monad.State
import Control.Concurrent.STM
--import Control.Concurrent.STM.TVar
import Control.Exception             (catch, SomeException)


-- https://jaspervdj.be/websockets/example/server.html?utm_source=chatgpt.com
-- https://blog.gordo.life/2026/04/12/haskell-websockets.html

------------------------------------------------------------------------------------------------
-- CLIENTS AND SERVER STATE
-- 
-- 
-- Server keeps track of two things at the same time:
--   * The poker game state (Table)
--   * The connected websocket clients
-- Both of these live together in ServerState and is shared across all client threads.
------------------------------------------------------------------------------------------------

-- | Unique ID assigned to each client connection when it connects.
--   Lets us track clients independently of whether they've joined the game yet.
type ClientId = Int

-- | Contains everything we need to know about a connected client.
--   Name is empty from start and then filled when a player sends a JoinMsg.
data ClientInfo = ClientInfo
    { clientName :: PlayerName          -- Chosen name of the player
    , connection :: WS.Connection       -- a live websocket connection to their client.
    }

-- | Map of: ClientID -> ClientInfo. Lookup structure for conected clients.
--   E.g: 0 -> ("Sam", conn1)
--        1 -> ("Bob", conn2)
--        2 -> (""   , conn3) <- connected but not yet joined.
--
--   We need this to reach specific clients and find out what name belongs to what client.
--   This helps us to send messages to specific clients or broadcast to everyone.
type Clients = Map ClientId ClientInfo


-- | Full server state: poker table and all connected clients.
data ServerState = ServerState 
    { table        :: Table     -- poker game state (engine)
    , clients      :: Clients   -- all connected websocket clients
    , nextClientId :: ClientId  -- counter to generate unique cIDs.
    , lobby        :: [PlayerName] -- queue of players waiting to join the game
    }

-- | A shared mutable state across all websocket threads.
--   Each connected client gets it's own thread, so the state must be shared safely.
--   TVar from STM gives us atomic reads and writes to avoid the risk of corruption.
--   Meaning if two clients were to act simultaneously then one will succeed and the other retry.
type SharedServerState = TVar ServerState

-- | Function to build the initial server state from a starting table.
--   No connected clients at this stage and cID counter starts at 0.
initServerState :: Table -> ServerState
initServerState t = 
    ServerState
        { table        = t          -- Engine owned initial table
        , clients      = M.empty    -- Websocket connections
        , nextClientId = 0          -- Client identifier
        , lobby        = []         -- Server owned, waiting queue
        }

------------------------------------------------------------------------------------------------
-- CLIENT HELPERS (LOOKUP)
--
-- Pure functions which query the serverstate but don't modify it. This formes the bridge
--- between websocket clientIDs and poker players in teh Table.
------------------------------------------------------------------------------------------------

-- | Check if a player name already used.
--   This rejects duplicate names when a player tries to join (we use the name as an identifier)

clientExists :: PlayerName -> ServerState -> Bool
clientExists n st = 
    any ((==n) . clientName) (M.elems (clients st))

-- | Lookup a clients info by their client ID, returns Nothing if not found.
lookupClient :: ClientId -> ServerState -> Maybe ClientInfo
lookupClient cid st =
    M.lookup cid (clients st)

-- | Lookup the players name associated with a client ID.
--   Returns nothing if the client hasn't sent a JoinMsg yet (name is still empty).
lookupPlayer :: ClientId -> ServerState -> Maybe PlayerName
lookupPlayer cid st = do
    info <- lookupClient cid st
    pure (clientName info)

-- | Find a players index in the Table's players list by name. 
--   (The engine works with indexes in the list, not names, so this translates.)
findPlayerIndex :: PlayerName -> Table -> Maybe PlayerIndex
findPlayerIndex n t =
    findIndex (\p -> Types.GameTypes.name p == n) (players t)

-- | Resolve a ClientId directly to a PlayerIndex in the table.
--   Websocket client -> player name -> engine index.
--   Returns Nothing if the client hasn't joined, or isn't in the table.
resolvePIdx :: ServerState -> ClientId -> Maybe PlayerIndex
resolvePIdx st cid = do
    n <- lookupPlayer cid st         -- ClientId -> PlayerName
    findPlayerIndex n (table st)     -- PlayerName -> PlayerIndex in table

------------------------------------------------------------------------------------------------
-- CLIENT STATE MODIFIERS
--
-- Return a new ServerState with modification applied.
--
------------------------------------------------------------------------------------------------

-- | Assign a player a name to an already connected client.
--   This is called when a JoinMsg arrives. The client entry exists as it was created on
--   connect, so we can just update the field in place.
nameClient :: ClientId -> PlayerName -> ServerState -> ServerState
nameClient cid playerName st =
    case lookupClient cid st of
        Nothing -> st   -- client wasn't found, return unchanged state.
        Just info -> st { clients = M.insert cid (info { clientName = playerName }) (clients st) }

-- | Remove a client from the connected clients map. This is called when a player leaves
--   or disconnects. Doesn't remove them from the Table as that is handled in a separate
--   function 'removePlayer' in the engine.
removeClient :: ClientId -> ServerState -> ServerState
removeClient cid st =
    st { clients = M.delete cid (clients st) }
                

------------------------------------------------------------------------------------------
-- WEBSOCKET SERVER ENTRY
--
-- WS.ServerApp is a type synonym for WS.PendingConnection -> IO ().
-- The WS library calls this function for every new client connection, for each thread.
-- The threads run concurrently.
------------------------------------------------------------------------------------------

-- | Accepts a new client connection, registers it in the server state, sends an initial snapshot
--   of the table, then goes to the main loop.
server :: SharedServerState -> WS.ServerApp
server stateVar pending = do
    putStrLn "Client trying to connect..."
    conn <- WS.acceptRequest pending
    putStrLn "Client connected!"

    -- This keeps the connection alive by sending pings every 30 sec.
    WS.forkPingThread conn 30

    -- Atomically allocate a unique client ID and register an entry placeholder.
    -- We do this atomically so no threads can get the same ID.
    -- Name is left empty until a JoinMsg arrives.
    cid <- atomically $ do
        st <- readTVar stateVar
        let cid = nextClientId st
            info = ClientInfo "" conn
        
        writeTVar stateVar st
            { clients = M.insert cid info (clients st)
            , nextClientId = cid + 1
            }
        pure cid

    -- Send current table state so client can render the exisiting game upon connection.
    st <- readTVarIO stateVar
    sendMsg conn (makeSnapshot (table st))

    putStrLn ("Assigned ClientId: " ++ show cid)

    -- Here the client goes to the main loop.
    serverLoop stateVar cid conn


-- atomically: means run this state as if no other thread exists. gives safety.
-- TVar: shared mutable state for multiple clients.

-- Atomic updates:
-- makes it safer with no risk of a corrupted state and safety for concurrent updates
-- where there are multiple websocket threads (clients). If 2 players act at the same time
-- (shouldn't be possible for our game anyway), then one succeeds and the other autmoatically retries.

-- Main server loop
-- Reads incoming JSON messages from a clinet, decodes them into events, routes them to the game engine.

------------------------------------------------------------------------------------------
-- MAIN SERVER LOOP
--
--
-- Each client runs an instance of this in their thread.
-- It waits for a message, handles it, updates the shared state (TVar) and repeats.
-- Also handles if the connection drops, receiveData throws an exception which is 
-- caught by 'onDisconnect'
------------------------------------------------------------------------------------------

serverLoop :: SharedServerState -> ClientId -> WS.Connection -> IO ()
serverLoop stateVar cid conn = loop `catch` onDisconnect
    where
        -- 
        loop = do
            msg <- WS.receiveData conn
            case Aeson.decode msg of
                -- Client sent something which was incorrect, output ErrorMsg and keep looping.
                Nothing -> do
                    sendMsg conn (ErrorMsg "Invalid JSON")
                    loop

                -- Received a valid message. Read the state, handle it and write the new state back, loop for next message.
                Just event -> do

                    -- read current shared state
                    st <- readState stateVar

                    -- handle event and produce updated state
                    newSt <- handleClientMsg st cid conn event

                    -- safely update shared state.
                    atomically $ writeTVar stateVar newSt

                    loop

        -- | Called when connection drops (the exception from redeiveData)
        --   IT will then remove the client from the shared state and then the player from the table.
        --   Then notifies the other clients that the player has left.
        onDisconnect :: SomeException -> IO ()
        onDisconnect _ = do
            putStrLn ("Client " ++ show cid ++ " disconnected")

            st <- readTVarIO stateVar

            case lookupPlayer cid st of
                -- Client connected but didn't join, remove them.
                Nothing -> pure ()
                -- Client joined the game as a player, then remove both from the client map and the players list in the Table.
                Just pName -> do
                    let st1 = removeClient cid st
                        st2 = st1 { table = removePlayer pName (table st1) }

                    atomically $ writeTVar stateVar st2

                    broadcastToClients st2 (LeftTable pName)
                    broadcastToClients st2 (makeSnapshot (table st2)) 

                    -- CHANGE TO ADD PLAYER TO GAME IF DISCONNECTED.


------------------------------------------------------------------------------------------
-- STATE HELPERS
------------------------------------------------------------------------------------------

-- txt :: String -> Text
-- txt = T.pack

-- Read shared server state
readState :: SharedServerState -> IO ServerState
readState = readTVarIO

-- -- Modify shared server state (strict)
-- modifyState :: SharedServerState -> (ServerState -> ServerState) -> IO ()
-- modifyState var f =
--     atomically $ modifyTVar' var f

------------------------------------------------------------------------------------------
-- MESSAGE HANDLER
--
--
-- This is the bridge between JSOON from clients and the poker engine itself.
-- Each pattern match handles a specific type of ClientMsg (similar to event handler in engine).
-- It returns an updated ServerState after handling, and the caller writes it to the TVar atomically.
------------------------------------------------------------------------------------------

handleClientMsg :: ServerState -> ClientId -> WS.Connection -> ClientMsg -> IO ServerState

----------------
-- PLAYER JOINS
-- 
-- A player wants to join the table. Reject duplicate names, then update client record and add them
-- to the engines player list ('addPlayer'). The client/player who joins receives a full (private) snapshot
-- and the other clients get a JoinedTable message and a public snapshot update.
----------------

handleClientMsg st cid _ (JoinMsg n)
    | clientExists n st = do
        sendToClientById cid st (ErrorMsg "Name already taken")
        pure st

    -- If table is full, put them in lobby.
    | isTableFull st = do
        let st1 = nameClient cid n st
            st2 = addPlayerToLobby n st1
        
        sendToClientById cid st2 (LobbyJoined n (length (lobby st2)))
        broadcastToClients st2 (LobbyUpdate (lobby st2))

        pure st2
    
    -- Table isn't full, seat them at table.
    | otherwise = do
        let st1 = nameClient cid n st
            st2 = st1 { table = addPlayer n (table st1) }

        -- Only the joining player receives the full snapshot, which is their private view. 
        sendToClientById cid st2 (makeSnapshot (table st2))

        -- Tel everyone else a player joined
        broadcastExcept cid st2 (JoinedTable n)

        -- and update everyone's public snapshot.
        broadcastExcept cid st2 (makePublicSnapshot (table st2))

        pure st2

------------------
-- PLAYER LEAVES
--
-- A player leaves the table. Removes them from both the client map and the engines player list.
-- Broadcasts to the other clients that the client left. This is only for clients who
-- explicitly leave, not disconnects.
------------------

handleClientMsg st cid _ LeaveMsg =
    case lookupPlayer cid st of
        Nothing -> pure st
        Just plName -> 
            if isInLobby plName st
                then do
                    -- Remove from lobby and remove client
                    let st1 = removeClient cid st
                        st2 = removePlayerFromLobby plName st1
                    
                    broadcastToClients st2 (LobbyUpdate (lobby st2))
                    pure st2
                else do
                    -- Remove from table and then move player from lobby to game.
                    let st1 = removeClient cid st
                        st2 = st1 { table = removePlayer plName (table st1) }

                    broadcastToClients st2 (LeftTable plName)
                    broadcastToClients st2 (makeSnapshot (table st2))

                    st3 <- moveFromLobbyToTable st2

                    pure st3


------------------
-- Player Action
--
-- A player takes an action in the game. 
-- First we resolve their ClientId to a playerindex in the table that the engine understands.
-- Then run the action through the engines 'applyEvent' function, and after broadcast the results to all clients.
--
------------------
handleClientMsg st cid conn (ActionMsg action) =
    case resolvePIdx st cid of
        Nothing -> do
            -- The client isn't a seated player, reject action.
            -- Safety measure incase the clients map and players list in engine aren't in sync. 
            -- (A player can be connected and not yet have a name, this means they wont be in the engines players list.)
            sendMsg conn (ErrorMsg "Not seated at the table")
            pure st

        Just pIdx -> do
            -- 'applyEvent' is a pure state functino. We run it against the current table and get
            -- either an error or a list of GameEvents, along with the updated table state.
            let event = PlayerEvent pIdx action

                (result, newTable) = runState (applyEvent event) (table st)
                            
            case result of
                -- The engine rejected the action. Tell only the player trying to make the
                -- action, and no state change.
                Left inv -> do
                    sendToClientById cid st (ErrorMsg inv)
                    pure st

                -- The action was valid. Broadcast to clients what happened (the GameEvents) and
                -- then update the clients with the new table state using a snapshot.
                Right gameEvents -> do
                    let newState = st { table = newTable }
                                    
                    broadcastToClients newState (GameEventMsgs gameEvents)
                    broadcastToClients newState (makeSnapshot newTable)

                    pure newState

------------------------------------------------------------------------------------------------
-- BROADCAST TO CLIENTS
------------------------------------------------------------------------------------------------

-- | Send a message to every connected client.
broadcastToClients :: ServerState -> ServerMsg -> IO ()
broadcastToClients st msg = do
    putStrLn ("Broadcasting to " ++ show (M.size (clients st)) ++ " clients")
    mapM_ (\info -> sendMsg (connection info) msg) (M.elems (clients st))

-- | Sends a message to everyone except one specific client.
--   This is used when someone joinds so we can notify them someone joined, but the joining client doesn't get this.
broadcastExcept :: ClientId -> ServerState -> ServerMsg -> IO ()
broadcastExcept excludedId st msg =
    mapM_ 
        (\(cid, info) -> 
            if cid /= excludedId
                then sendMsg (connection info) msg
                else pure ())
        (M.toList (clients st))


-- | Send a message to one specific client using their ClientID.
--   This is used for private messages like initial snapshot on join or errormessages.
sendToClientById :: ClientId -> ServerState -> ServerMsg -> IO ()
sendToClientById cid st msg =
    case M.lookup cid (clients st) of
        Nothing -> pure ()
        Just info -> sendMsg (connection info) msg

-- | Encode ServerMsg as JSON and send over ws connection.
sendMsg :: WS.Connection -> ServerMsg -> IO ()
sendMsg conn msg =
    WS.sendTextData conn (Aeson.encode msg)


------------------------------------------------------------------------------------------------
-- LOBBY ( QUEUE OF PLAYERS WAITING AT TO JOIN TABLE )
--
--
--
------------------------------------------------------------------------------------------------

-- | Constant for max amount of players at table
maxSeats :: Int
maxSeats = 6

-- | Check whether table is full
isTableFull :: ServerState -> Bool
isTableFull st = length (players (table st)) >= maxSeats

-- | Add a player to the lobby waiting list.
--   Added to end of queue.
addPlayerToLobby :: PlayerName -> ServerState -> ServerState
addPlayerToLobby plName st = st { lobby = lobby st ++ [plName] } 

-- | Remove a player from the lobby.
removePlayerFromLobby :: PlayerName -> ServerState -> ServerState
removePlayerFromLobby plName st = st { lobby = filter (/= plName) (lobby st) }

-- | Check if a player is in the lobby queue.
isInLobby :: PlayerName -> ServerState -> Bool
isInLobby n st = n `elem` lobby st

-- | Move the first player in the lobby to the table if there is any space. Called when a player
--   leaves or is eliminated.
moveFromLobbyToTable :: ServerState -> IO ServerState
moveFromLobbyToTable st = 
    case lobby st of
        []    -> pure st -- lobby is empty.
        (n:_) -> 
            if isTableFull st
                then pure st    -- Table is still full of players
                else do         -- Table is not full, add player to players list.
                    let st1 = removePlayerFromLobby n st
                        st2 = st1 { table = addPlayer n (table st1) }
                    
                    broadcastToClients st2 (JoinedTable n)
                    broadcastToClients st2 (makeSnapshot (table st2))
                    broadcastToClients st2 (LobbyUpdate (lobby st2))

                    pure st2

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
    , snapDealer  = dealerPosition t
    }
    where
        -- | Convert a player in the engine to a PlayerSnapshot that is safe to broadcast.
        --   snapHand hidden, should not be broadcasted publicly.
        toPlayerSnap p = PlayerSnapshot
            { snapName = name p
            , snapChips = chips p
            , snapCommited = commitedChips p
            , snapFolded   = folded p
            , snapHand     = Nothing
            }

