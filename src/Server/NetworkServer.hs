{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Server.NetworkServer
Description : WebSocket server and client connection management.

This module is responsible for accepting WebSocket connections, maintaining the shared server state,
and handling incoming messages from clients. It translates client messages into game transitions and executes
broadcast actions returned by the game loop.

Responsibilities summarised:

* Accepting incoming WebSocket connections and registering clients in 'ServerState'
* Running a per-client receive loop that decoded incomsing JSON messages.
* Translating 'ClientMsg' values into 'Transition' values for 'Server.GameLoop'
* Handling 'Client' disconnects.
* Executing 'BroadcastAction's returned by 'Server.GameLoop' to send appropriate messages to clients.

The module is kept free from game logic. All game related state transitions are delegated to 'Server.GameLoop' via 'handleTransition'.

-}
module Server.NetworkServer 
    ( -- * WebSocket server entry
      -- | WS.ServerApp is a type synonym for WS.PendingConnection -> IO ().
      -- The WS library calls 'server' once per client connection each on its
      -- own thread and runs concurrently per client.
      server

      -- * Client receive loop and disconnect handling
      -- | Each client runs an instance of this in their thread.
      -- It waits for a message, handles it, updates the shared state (TVar) and repeats.
      -- If the connection drops, WS.receiveData throws an exception which is 
      -- caught by 'onDisconnect'
    , serverLoop

      -- * Message translation
    , toTransition

      -- * Broadcast actions execution
      -- | GameLoop returns a list of BroadcastActions that describe what should be sent and to who.
      -- The functions bewlos executes those actions by sending over the actual WS connections.
    , executeActions
    , executeAction
    , broadcastMsg
    , broadcastToAllClients
    , sendToClientById
    , broadcastToAllClientsExcept
    
      -- * State helpers
    , readState
    ) where

import Engine.Utilities (removePlayer)
import Server.Protocol
import Server.ServerTypes
import Server.ServerHelpers
import Server.GameLoop

import qualified Network.WebSockets  as WS
import qualified Data.Aeson          as Aeson
import qualified Data.Map.Strict     as M

import Control.Concurrent.STM        (atomically, readTVar, readTVarIO, writeTVar)
import Control.Exception             (catch, SomeException)



-- * WebSocket server entry

-- | The WS.ServerApp handler. Called once per incoing connection on its own thread.
--
-- On each new connection:
--
-- * Accepts the pending connection and starts a ping thread to keep it alive.
-- * Atomically allocates a unique 'ClientId' and registers a placeholder 'ClientInfo' with an empty name in the shared 'ServerState'.
-- * Sends the new client a 'Snapshot' of the current 'Table' state so they can render the existing game immediately.
-- * Hands off to 'serverLoop' to begin receiving messages.
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


-- * Client receive loop and disconnect handling

-- | Main receive loop for a connected client.
--
-- On each iteration::
--
-- * Waits for a message from the client via 'WS.receiveData'
-- * Attempts to decode the message into 'ClientMsg'. Sends 'ErrorMsg' and continues if decoding failed.
-- * Reads the current 'ServerState' , translate the 'ClientMsg' into a 'Transition' via 'toTransition' and delegates to 'handleTransition'.
-- * Atomically writes the new state back to the 'TVar' and executes any returned 'BroadcastAction's.
--
-- If the connection drop,s 'WS.receiveData' throws and exception which is caught by 'onDisconnect'.
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

                    st <- readState stateVar

                    -- convert client message to transition and handle it
                    let transition = toTransition cid clientMsg
                    (newSt, actions) <- handleTransition st transition

                    -- safely update shared state.
                    atomically $ writeTVar stateVar newSt

                    -- execute all broadcast actions
                    executeActions newSt actions

                    loop

        -- | Called when a WS connection drops (the exception from redeiveData)
        --   Remove the client from the shared state, lobby, and table.
        --   Then notifies the other clients that the player has left.
        onDisconnect :: SomeException -> IO ()
        onDisconnect e = do
            putStrLn ("Client " ++ show cid ++ " disconnected: " ++ show e)

            st <- readTVarIO stateVar

            case lookupPlayer cid st of
                -- Client connected but didn't send a JoinMsg, remove them.
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


-- * Message translation

-- | Translates a 'ClientMsg' into a 'Transition' for 'Server.GameLoop'
--   This is hte only place that knows about both types to keep 'Server.NetworkServer'
--   and 'Server.GameLoop' decoupled from each other.
toTransition :: ClientId -> ClientMsg -> Transition
toTransition cid (JoinMsg n)     = PlayerJoined cid n
toTransition cid LeaveMsg        = PlayerLeft cid
toTransition cid (ActionMsg act) = PlayerActed cid act



-- * Broadcast actions execution

-- | Execute a list of broadcast actions. Send messages to appropriate clients.
executeActions :: ServerState -> [BroadcastAction] -> IO ()
executeActions st actions = mapM_ (executeAction st) actions

-- | Execute a single broadcast action.
executeAction :: ServerState -> BroadcastAction -> IO ()
executeAction st action = case action of
    BroadcastToAll msg              -> broadcastToAllClients st msg
    SendToClient cid msg            -> sendToClientById cid st msg
    BroadcastToAllExcept exclID msg -> broadcastToAllClientsExcept exclID st msg

-- | Encode ServerMsg as JSON and send over a WS connection.
broadcastMsg :: WS.Connection -> ServerMsg -> IO ()
broadcastMsg conn msg = WS.sendTextData conn (Aeson.encode msg)

-- | Send a message to all connected clients.
broadcastToAllClients :: ServerState -> ServerMsg -> IO ()
broadcastToAllClients st msg = do
    putStrLn ("Broadcasting to " ++ show (M.size (clients st)) ++ " clients")   -- Terminal output.
    mapM_ (\info -> broadcastMsg (connection info) msg) (M.elems (clients st))

-- | Send a message to a single client by their ClientId.
sendToClientById :: ClientId -> ServerState -> ServerMsg -> IO ()
sendToClientById cid st msg =
    case M.lookup cid (clients st) of
        Nothing -> pure ()
        Just info -> broadcastMsg (connection info) msg

-- | Send a message to all clients except one with the given ClietnId.
broadcastToAllClientsExcept :: ClientId -> ServerState -> ServerMsg -> IO ()
broadcastToAllClientsExcept excludedId st msg =
    mapM_ 
        (\(cid, info) -> 
            if cid /= excludedId
                then broadcastMsg (connection info) msg
                else pure ())
        (M.toList (clients st))


-- * State helpers

-- Reads the current server state form the shared TVar.
readState :: SharedServerState -> IO ServerState
readState = readTVarIO


