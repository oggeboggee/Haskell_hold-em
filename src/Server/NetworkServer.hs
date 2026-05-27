{-# LANGUAGE OverloadedStrings #-}

module Server.NetworkServer where


import Engine.TexasEngine 
import Server.Protocol
import Server.ServerTypes
import Server.ServerHelpers
import Server.GameLoop

import qualified Network.WebSockets  as WS
import qualified Data.Aeson          as Aeson
import qualified Data.Map.Strict     as M


import Control.Concurrent.STM        (atomically, readTVar, readTVarIO, writeTVar)
import Control.Exception             (catch, SomeException)

------------------------------------------------------------------------------------------
-- WEBSOCKET SERVER ENTRY
--
-- WS.ServerApp is a type synonym for WS.PendingConnection -> IO ().
-- The WS library calls 'server' once per client connection each on its
-- own thread and runs concurrently per client.
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
    -- We  store immediately so 'onDisconnect' alwasy can find the client.
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

    broadcastMsg conn (makeSnapshot (table st))

    putStrLn ("Assigned ClientId: " ++ show cid)

    -- Here the client goes to the main loop.
    serverLoop stateVar cid conn



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
                    broadcastMsg conn (ErrorMsg "Invalid JSON")
                    loop

                -- Received a valid message. Read the state, handle it and write the new state back, loop for next message.
                Just clientMsg -> do

                    -- read current shared state
                    st <- readState stateVar

                    -- convert client message to transition and handle it
                    let transition = toTransition cid clientMsg
                    (newSt, actions) <- handleTransition st transition

                    -- safely update shared state.
                    atomically $ writeTVar stateVar newSt

                    -- execute all broadcast actions
                    executeActions newSt actions

                    loop

        -- | Called when connection drops (the exception from redeiveData)
        --   IT will then remove the client from the shared state and then the player from the table.
        --   Then notifies the other clients that the player has left.
        onDisconnect :: SomeException -> IO ()
        onDisconnect e = do
            putStrLn ("Client " ++ show cid ++ " disconnected: " ++ show e)

            st <- readTVarIO stateVar

            case lookupPlayer cid st of
                -- Client connected but didn't join, remove them.
                Nothing -> pure ()
                -- Client joined the game as a player, then remove both from the client map and the players list in the Table.
                Just pName -> do
                    let st1 = removeClient cid st
                        st2 = removePlayerFromLobby pName st1
                        st3 = st2 { table = removePlayer pName (table st2) }

                    atomically $ writeTVar stateVar st3
                    
                    -- Execute disconnect broadcast actions
                    let actions =
                            [ BroadcastToAll (LeftTable pName)
                            , BroadcastToAll (makeSnapshot (table st3))
                            , BroadcastToAll (LobbyUpdate (lobby st3))
                            ]
                    executeActions st3 actions



-- | Translate a ClientMsg into a transition for the orchestrator.
--   This is the only place that knows about ClientMsg and Transition
toTransition :: ClientId -> ClientMsg -> Transition
toTransition cid (JoinMsg n) = 
    PlayerJoined cid n

toTransition cid LeaveMsg = 
    PlayerLeft cid

toTransition cid (ActionMsg act) = 
    PlayerActed cid act


------------------------------------------------------------------------------------------------
-- EXECUTE BROADCAST ACTIONS
--
-- Takes the broadcast actions produced by GameLoop and executes them by sending messages
-- over WebSocket connections. This is the networking layer that GameLoop doesn't know about.
------------------------------------------------------------------------------------------------

-- | Execute a list of broadcast actions. Send messages to appropriate clients.
executeActions :: ServerState -> [BroadcastAction] -> IO ()
executeActions st actions = mapM_ (executeAction st) actions

-- | Execute a single broadcast action.
executeAction :: ServerState -> BroadcastAction -> IO ()
executeAction st action = case action of
    BroadcastToAll msg ->
        broadcastToAllClients st msg

    SendToClient cid msg ->
        sendToClientById cid st msg

    BroadcastToAllExcept excludedId msg ->
        broadcastToAllClientsExcept excludedId st msg

-- | Encode ServerMsg as JSON and send over ws connection.
broadcastMsg :: WS.Connection -> ServerMsg -> IO ()
broadcastMsg conn msg =
    WS.sendTextData conn (Aeson.encode msg)

-- | Send a message to all connected clients.
broadcastToAllClients :: ServerState -> ServerMsg -> IO ()
broadcastToAllClients st msg = do
    putStrLn ("Broadcasting to " ++ show (M.size (clients st)) ++ " clients")
    mapM_ (\info -> broadcastMsg (connection info) msg) (M.elems (clients st))

-- | Send a message to a single client by their ClientId.
sendToClientById :: ClientId -> ServerState -> ServerMsg -> IO ()
sendToClientById cid st msg =
    case M.lookup cid (clients st) of
        Nothing -> pure ()
        Just info -> broadcastMsg (connection info) msg

-- | Send a message to all clients except one.
broadcastToAllClientsExcept :: ClientId -> ServerState -> ServerMsg -> IO ()
broadcastToAllClientsExcept excludedId st msg =
    mapM_ 
        (\(cid, info) -> 
            if cid /= excludedId
                then broadcastMsg (connection info) msg
                else pure ())
        (M.toList (clients st))


------------------------------------------------------------------------------------------
-- STATE HELPERS
------------------------------------------------------------------------------------------

-- Read shared server state
readState :: SharedServerState -> IO ServerState
readState = readTVarIO


