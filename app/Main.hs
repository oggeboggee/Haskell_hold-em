{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-tabs #-}
module Main where

import Engine.Cards
import Engine.EngineTypes
import Server.NetworkServer

import qualified Network.WebSockets as WS
import Control.Concurrent.STM
import Server.ServerTypes

-- | Entry point for the poker WebSocket server.
--   Initializes the game state and starts the server on port 9160.
main :: IO ()
main = do
	let initialTable = Table
		{ players            = []
        , deck               = fullDeck
        , board              = []
        , phase              = PreFlop
        , highBet            = 0
        , pot                = 0
        , dealerPosition     = 0
        , smallBlindPosition = 0
        , bigBlindPosition   = 1
        , bets = []
        }         
    
	putStrLn "Poker server running on ws://localhost:9160"

	stateVar <- newTVarIO (initServerState initialTable)
    
	WS.runServer "127.0.0.1" 9160 (server stateVar)
