module Main where

import TexasLogic
    ( Table(Table, phase, players, deck, board),
      Player(Player),
      GamePhase(DealHands),
      gameLoop )
import Cards


main :: IO ()
main = do
    putStrLn "Welcome to Haskell Hold'em!"

    let initialTable = Table {
            players = [Player "Alice" []],
            deck = fullDeck,
            board = [],
            phase = DealHands
        }

    gameLoop initialTable
