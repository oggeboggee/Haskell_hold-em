module Main where

import System.IO 
import Cards
import TexasLogic


main :: IO ()
main = do
    hSetEncoding stdout utf8
    --hSetEncoding stderr utf8
    putStrLn "Welcome to Haskell Hold'em!"

    let initialTable = Table {
        players = [Player "Bob" [], Player "Sam" []],
        deck = fullDeck,
        board = [],
        phase = DealHands
    }

    gameLoop initialTable