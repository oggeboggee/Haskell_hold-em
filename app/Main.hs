module Main where

import Engine.Cards
import Engine.TexasEngine
import Types.GameTypes
import Engine.TerminalUI
import Server.TestJson
import Server.NetworkServer

import System.IO ()

import qualified Network.WebSockets as WS


import Control.Monad.State
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

main :: IO ()
main = do


    let initPlayers = [Player "Bob" [] 1000 0 False False,
                       Player "Sam" [] 1000 0 False False,
                       Player "Jonathan" [] 1000 0 False False,
                       Player "Lewis" [] 1000 0 False False]

        initPlayers2 = [Player "Bob" [] 1000 0 False False,
                        Player "Sam" [] 1000 0 False False]
                   
        initialTable = Table 
            { players = initPlayers2,
              deck = fullDeck,
              board = [],
              phase = DealHands,
              highBet = 0,
              pot = 0,
              dealerPosition = 0,
              smallBlindPosition = 0,
              bigBlindPosition = 1,
              bets = []
            }
    
    putStrLn "Poker server running on ws://localhost:9160"
    stateVar <- newTVarIO (initServerState initialTable)
    WS.runServer "127.0.0.1" 9160 (server stateVar)
{-
    putStrLn "\nWelcome to Haskell Hold'em!"


    let initPlayers = [Player "Bob" [] 1000 0 False False,
                       Player "Sam" [] 1000 0 False False,
                       Player "Jonathan" [] 1000 0 False False,
                       Player "Lewis" [] 1000 0 False False]

        initPlayers2 = [Player "Bob" [] 1000 0 False False,
                        Player "Sam" [] 1000 0 False False]
                   
        initialTable = Table 
            { players = initPlayers2,
              deck = fullDeck,
              board = [],
              phase = DealHands,
              highBet = 0,
              pot = 0,
              dealerPosition = 0,
              smallBlindPosition = 0,
              bigBlindPosition = 1,
              bets = []
            }

--  | Evaluate a state computation with the given initial state and return the final value, 
--    discarding the final state
--    https://hackage-content.haskell.org/package/mtl-2.3.2/docs/Control-Monad-State-Lazy.html
  --  evalStateT gameRound initialTable

    --runJsonTest

-}
-- Init table for Axel
{-
let players = [Player "Bob" [] 1000 0 False False SmallBlind False,
                   Player "Sam" [] 1000 0 False False BigBlind False,
                   Player "Frodo" [] 1000 0 False False NoBlind False]
-}