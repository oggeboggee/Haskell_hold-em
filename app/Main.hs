module Main where

import System.IO ()
import Cards
import TexasEngine
import Types
import Control.Monad.State


main :: IO ()
main = do

    putStrLn "\nWelcome to Haskell Hold'em!"

    let initPlayers = [Player "Bob" [] 1000 0 False False,
                       Player "Sam" [] 1000 0 False False,
                       Player "Jonathan" [] 1000 0 False False,
                       Player "Lewis" [] 1000 0 False False]
                   
        initialTable = Table 
            { players = initPlayers,        
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
    evalStateT gameRound initialTable

