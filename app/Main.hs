module Main where

import Cards
import TexasLogic


main :: IO ()
main = do
    putStrLn "Welcome to Haskell Hold'em!"

    let initialTable = Table {
        players = [Player "Bob" []],
        deck = fullDeck,
        board = [],
        phase = DealHands
    }

    gameLoop initialTable