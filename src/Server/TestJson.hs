module Server.TestJson where

import Types.GameTypes

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.ByteString.Lazy.Char8 as BSL

runJsonTest :: IO ()
runJsonTest = do
    let event = PlayerEvent 0 (Raise 200)

    BSL.putStrLn (encodePretty event)

    let encoded = encode event

    print (decode encoded :: Maybe Event)