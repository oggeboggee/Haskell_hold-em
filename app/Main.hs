module Main where

import System.IO 
import Cards
import TexasEngine
import Types
import Control.Monad.State


main :: IO ()
main = do

    putStrLn "Welcome to Haskell Hold'em!"

    let players = [Player "Bob" [] 1000 0 False False NoBlind False,
                   Player "Sam" [] 1000 0 False False SmallBlind False,
                   Player "Frodo" [] 1000 0 False False BigBlind False]
                   
        initialTable = Table 
            { players = players,
              --activePlayers = players,           
              deck = fullDeck,
              board = [],
              phase = DealHands,
              highBet = 0,
              pot = 0,
              dealerPosition = 0,
              smallBlindPosition = 1,
              bigBlindPosition = 2,
              bets = []
            }

--  | Evaluate a state computation with the given initial state and return the final value, 
--    discarding the final state
--    https://hackage-content.haskell.org/package/mtl-2.3.2/docs/Control-Monad-State-Lazy.html
    evalStateT gameLoop initialTable

