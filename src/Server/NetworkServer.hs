
{-# LANGUAGE OverloadedStrings #-}

module Server.NetworkServer where

import Types.GameTypes
import Engine.Actions
import Engine.TexasEngine
import Engine.Actions

import qualified Network.WebSockets as WS
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import Data.Text (Text)


import Control.Monad.State
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar


-- https://jaspervdj.be/websockets/example/server.html?utm_source=chatgpt.com
-- https://blog.gordo.life/2026/04/12/haskell-websockets.html

---------------------------------------------------------------------------------------
-- CLIENTS AND SERVER STATE
----------------------------------------------------------------------------------------

-- A connected websocket client. Store the players chosen name and their websocket connection.
type Client = (PlayerName, WS.Connection)

-- A shared mutable state that is used by all websocket threads.
-- Every connected browser (client) gets it's own thread, so the state
-- must then be shared safely. TVar comes from STM.
type SharedServerState = TVar ServerState

-- Server state contains:
-- 1. The current table
-- 2. all connected clients.
data ServerState = ServerState 
    { table :: Table
    , clients :: [Client]
    }

-- An initial serverstate
initServerState :: Table -> ServerState
initServerState t = ServerState
    { table = t
    , clients = []
    }

------------------------------------------------------------------------------------------
-- Client helpers
-------------------------------------------------------------------------------------------


-- Check if a player name already exists in the server.
clientExists :: PlayerName -> [Client] -> Bool
clientExists name = 
    any (\(n, _) -> n == name)

-- Add a new client to the server state
addClient :: PlayerName -> WS.Connection -> ServerState -> ServerState
addClient name conn st = 
    st { clients = (name, conn) : clients st }

-- remove a client from the server state
removeClient :: PlayerName -> ServerState -> ServerState
removeClient name st =
    st { clients = filter (\(n, _) -> n /= name) (clients st) }


------------------------------------------------------------------------------------------
-- WEBSOCKET SERVER
------------------------------------------------------------------------------------------

-- main websocket server entry point.

-- WS.ServerApp is a type synonym for WS.PendingConnection -> IO ().

-- Every new browswer connection get its own thread.
server :: SharedServerState -> WS.ServerApp
server stateVar pending = do
    putStrLn "Client trying to connect..."

    conn <- WS.acceptRequest pending
    putStrLn "Client connected!"

    WS.forkPingThread conn 30

    serverLoop stateVar conn


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
------------------------------------------------------------------------------------------
-- The main websocket loop. It continuosly:

-- 1. Waits for JSON from the browser.
-- 2. Decoded it into an event
-- 3. handles the event
-- 4. updates the SharedServerState (TVar).
-- 5. loops

serverLoop :: SharedServerState -> WS.Connection -> IO ()
serverLoop stateVar conn = do
    msg <- WS.receiveData conn

    case decode msg of
        -- INVALID JSON
        Nothing -> do
            WS.sendTextData conn ("Invalid JSON" :: Text)
            serverLoop stateVar conn

        -- VALID JSON (event)
        Just event -> do

            -- read current shared state
            state <- readTVarIO stateVar

            -- handle event and produce updated state
            newState <- handleServerEvent state conn event

            -- safely update shared state.
            atomically $ writeTVar stateVar newState

            putStrLn ("EVENT: " ++ show event)

            serverLoop stateVar conn

------------------------------------------------------------------------------------------
-- MAIN SERVER LOOP
------------------------------------------------------------------------------------------

txt :: String -> Text
txt = T.pack

-- Read shared server state
readState :: SharedServerState -> IO ServerState
readState = readTVarIO

-- Modify shared server state (strict)
modifyState :: SharedServerState -> (ServerState -> ServerState) -> IO ()
modifyState var f =
    atomically $ modifyTVar' var f

------------------------------------------------------------------------------------------
-- EVENT
------------------------------------------------------------------------------------------

-- Handles incoming websocket events. Bridge between JSON from browser -> the poker engine -> broadcasted game evetns.
handleServerEvent :: ServerState -> WS.Connection -> Event -> IO ServerState
handleServerEvent state conn event =
    case event of

        -- PLAYER JOINS
        JoinEvent name -> do
            if clientExists name (clients state)
                then do
                    WS.sendTextData conn ("Name taken" :: Text)
                    return state
                else do
                    let newState =
                            addClient name conn $
                                state { table = addPlayer name (table state) }
                    WS.sendTextData conn ("Joined table" :: Text)
                    return newState

        -- PLAYER LEAVES
        LeaveEvent name -> do
            let newState = removeClient name state
            
            WS.sendTextData conn ("Left table:" :: Text)
            return newState


        -- PlayerEvents at table
        PlayerEvent idx action -> do
            let currentTable = table state


                (result, newTable) = runState (applyEvent (PlayerEvent idx action)) currentTable

            case result of
                -- invalid move
                Left err -> do
                    WS.sendTextData conn (txt err)
                    return state
                
                --valid game event
                Right gameEvents -> do
                    let json = encode gameEvents
                        newState = state { table = newTable }

                    putStrLn "=== GAME EVENTS ==="
                    print gameEvents
                    BSL.putStrLn json

                    -- send events to all connected clients.
                    broadcastToClients json newState

                    return newState

-- Send a websocket message to every connected client.
broadcastToClients :: BL.ByteString -> ServerState -> IO ()
broadcastToClients msg st =
    mapM_ (\(_, conn) -> WS.sendTextData conn msg) (clients st)



-- TESTING

{-
To test:
Open two separate test.html files in chrome.
Then open devtools and console in each window.

then add each player:

ws.send(JSON.stringify({
    tag: "JoinEvent",
    contents: "name here"
}));

then test a playerEvent

ws.send(JSON.stringify({
    tag: "PlayerEvent",
    contents: [0, { tag: "Raise", contents: 200 }]
}));

It will say Bob raised for both players with how its set up now but we can see the events in each browser
and here in the terminal. Websocket user != poker player in the game right now.




Note: we need better structured JSON messages. we also should separate events for this from the events in the game.
      The engine shouldn't know about the server. we have high coupling. 

-}