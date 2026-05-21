--{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Protocol 
    ( ClientMsg(..)
    , ServerMsg(..)
    , PlayerSnapshot(..)
    ) where


import Data.Aeson

import Types.GameTypes

----------------------------------------------------------------------------------------
-- This module defines all messages that travel between clients and the server.
-- It also contains the JSON serialisation instances for engine types (Action, GameEvent, Card, GamePhase)
-- so they can be sent.
-- We have it here because the engine has no dependency on Aeson. JSON is not an engine concern.
----------------------------------------------------------------------------------------

-- Every message the client can send. Server decodes from JSON.
data ClientMsg
    = JoinMsg PlayerName    -- player wants to sit down: {"type":"join","name":"Alice"}
    | LeaveMsg              -- player leaves:            {"type":"leave"}
    | ActionMsg Action      -- player takes an action:   {"type":"action","action":{...}}
    deriving (Show)

-- Encode ClientMSg to JSON (never gets sent, mostly for debugging purposes)
instance ToJSON ClientMsg where
    toJSON (JoinMsg n)     = object ["type" .= ("join" :: String), "name" .= n]
    toJSON LeaveMsg           = object ["type" .= ("leave" :: String)]
    toJSON (ActionMsg action) = object ["type" .= ("action" :: String), "action" .= ActionJSON action] -- Wrap before encoding

-- Decode JSON from client into a ClientMsg. Every message must have a "type" field so we know how to parse.
instance FromJSON ClientMsg where
    parseJSON = withObject "ClientMsg" $ \o -> do
        t <- o .: "type"
        case (t :: String) of
            "join"   -> JoinMsg <$> o .: "name"
            "leave"  -> pure LeaveMsg
            "action" -> do
                ActionJSON act <- o .: "action" -- unwrap.
                pure (ActionMsg act)
            _        -> fail ("Unknown message type: " ++ t)


----------------------------------------------------------------------------------------
-- MESSAGES FROM SERVER TO CLIENT
----------------------------------------------------------------------------------------

-- A summary of one players state, safe to broadcast to other clients.
-- In a private snapshot we can send a player their cards.
data PlayerSnapshot = PlayerSnapshot
    { snapName     :: PlayerName
    , snapChips    :: Bet
    , snapCommited :: Int
    , snapFolded   :: Bool
    , snapHand     :: Maybe [String] -- Nothing = hidden, Just [..] visible
    } deriving (Show)

-- Encode a PlayerSnapShot to JSON for the client
instance ToJSON PlayerSnapshot where
    toJSON p = object
        [ "name"     .= snapName p
        , "chips"    .= snapChips p
        , "commited" .= snapCommited p
        , "folded"   .= snapFolded p
        , "hand"     .= snapHand p    -- JSON null when Nothing
        ]

-- All messages the server can send to a client.
-- Each constructor maps to a distinct "type" field in the JSON.
data ServerMsg
    = Welcome PlayerName            -- sent to a player when they first connect.
    | ErrorMsg String               -- something that went wrong: invalid action, name already taken etc.
    | JoinedTable PlayerName        -- broadcast when any player joins
    | LeftTable PlayerName          -- broadcast when any player leaves
    | GameEventMsgs [GameEvent]     -- what just happened (players folded, raised, shwodown etc.)
    | LobbyJoined PlayerName Int    -- Player joins lobby, at position
    | LobbyUpdate [PlayerName]      -- current lobby queue
    | Snapshot                      -- full public table state, sent after every state change.
        { snapPlayers :: [PlayerSnapshot]
        , snapPot     :: Chip
        , snapPhase   :: String
        , snapBoard   :: [String]
        , snapHighBet :: Bet
        , snapDealer  :: Int
        }
    deriving (Show)

-- Encode a ServerMsg to JSON. The client pattern matches on "type" to know what it received and how to render.
instance ToJSON ServerMsg where
    toJSON (Welcome n) =
        object ["type" .= ("welcome" :: String), "name" .= n]

    toJSON (ErrorMsg err) =
        object ["type" .= ("error" :: String), "message" .= err]

    toJSON (JoinedTable n) =
        object ["type" .= ("player_joined" :: String), "name" .= n]

    toJSON (LeftTable n) =
        object ["type" .= ("player_left" :: String), "name" .= n]

    toJSON (GameEventMsgs events) =
        object ["type" .= ("game_events" :: String), "events" .= map GameEventJSON events] -- Wrap each GameEvent to serialise it.

    toJSON (LobbyJoined n pos) =
        object ["type"      .= ("lobby_joined" :: String)
               , "name"     .= n
               , "position" .= pos
               ]

    toJSON (LobbyUpdate ns) =
        object ["type"   .= ("lobby_update" :: String)
               , "queue" .= ns
               ]

    toJSON s@(Snapshot{}) =
        object
            [ "type"     .= ("snapshot" :: String)
            , "players"  .= snapPlayers s
            , "pot"      .= snapPot s
            , "phase"    .= snapPhase s     -- ToJSON GamePhase below
            , "board"    .= snapBoard s     -- ToJSON Card below
            , "high_bet" .= snapHighBet s
            , "dealer"   .= snapDealer s
            ]


----------------------------------------------------------------------------------------
--  SERIALISATION FOR ENGINE TYPES
--
--
-- These instances lets us send engine types over JSON without the engine knowing about JSON.
----------------------------------------------------------------------------------------

-- | Wrapper for Action to handle JSON in a way that promotes SoC and avoids orphan instances.
newtype ActionJSON = ActionJSON Action

-- | Encode an Action taken by a player.
--   Each action type becomes a JSON object with a "type" field.
instance ToJSON ActionJSON where
    toJSON (ActionJSON Check)     = object ["type" .= ("check"  :: String)]
    toJSON (ActionJSON Fold)      = object ["type" .= ("fold"   :: String)]
    toJSON (ActionJSON Call)      = object ["type" .= ("call"   :: String)]
    toJSON (ActionJSON AllIn)     = object ["type" .= ("allin"  :: String)]
    toJSON (ActionJSON (Raise x)) = object ["type" .= ("raise"  :: String), "amount" .= x]

-- | Decode an Action from the client.
--   The "type" tells us what type of action and we can parse the rest.
instance FromJSON ActionJSON where
    parseJSON = withObject "Action" $ \o -> do
        t <- o .: "type"
        case (t :: String) of
            "check" -> pure (ActionJSON Check)
            "fold"  -> pure (ActionJSON Fold)
            "call"  -> pure (ActionJSON Call)
            "allin" -> pure (ActionJSON AllIn)
            "raise" -> ActionJSON . Raise <$> o .: "amount"
            _       -> fail ("Unknown action: " ++ t)

newtype CardJSON = CardJSON Card

-- | We encode a Card as a simple string which the show instance gives us.
instance ToJSON CardJSON where
    toJSON (CardJSON c) = toJSON (show c) 

newtype GamePhaseJSON = GamePhaseJSON GamePhase

-- | Same for GamePhase as Card
instance ToJSON GamePhaseJSON where
    toJSON (GamePhaseJSON p) = toJSON (show p)

newtype GameEventJSON = GameEventJSON GameEvent

-- | Encode what happened during a round.
--   Each variant described a different event with relevant context.
instance ToJSON GameEventJSON where
    toJSON (GameEventJSON (PlayerFolded n)) =
        object ["type" .= ("folded"  :: String), "player" .= n]

    toJSON (GameEventJSON (PlayerChecked n)) =
        object ["type" .= ("checked" :: String), "player" .= n]

    toJSON (GameEventJSON (PlayerCalled n amt)) =
        object ["type" .= ("called"  :: String), "player" .= n, "amount" .= amt]

    toJSON (GameEventJSON (PlayerRaised n amt)) =
        object ["type" .= ("raised"  :: String), "player" .= n, "amount" .= amt]

    toJSON (GameEventJSON (PlayerAllIn n amt)) =
        object ["type" .= ("allin"   :: String), "player" .= n, "amount" .= amt]

    toJSON (GameEventJSON (PlayerPlacedBlinds n bt amt)) =
        object ["type"       .= ("blind"   :: String)
               , "player"    .= n
               , "blind_type".= show bt    -- SmallBlind or BigBlind
               , "amount"    .= amt
               ]

    toJSON (GameEventJSON (ShowdownHappened winners)) =
        object ["type"    .= ("showdown" :: String)
               , "winners" .= winners
               ]
