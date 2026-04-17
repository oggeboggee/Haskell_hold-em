module Main where

import System.IO 
import Cards
import TexasEngine


main :: IO ()
main = do

    putStrLn "Welcome to Haskell Hold'em!"

    let initialTable = Table {
        players = [Player "Bob" [], Player "Sam" []],
        deck = fullDeck,
        board = [],
        phase = DealHands
    }

    gameLoop initialTable

