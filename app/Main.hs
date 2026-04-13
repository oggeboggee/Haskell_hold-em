module Main where

import Cards
import TexasLogic


main :: IO ()
main = do
    let p1 = Player "P1" []
    let p2 = Player "P2" []

    let deck = fullDeck

    let startingTable = Table {

    }
