--{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Server.Protocol
Description : JSON definitions for messages sent between clients and server.

This module defines the network protocol used by the poker server. It contains the
data types for all messages sent between clients and the server, as well as the JSON
serialisation instances required to encode and decode them.

* 'ClientMsg' - messages sent from client to server.
* 'ServerMsg' - messages sent from server to client.

The module also provides JSON wrapper types for engine data types such as 'Action', 'GameEvent',
'Card, and 'GamePhase'. These wrappers allow for engine types to be serialised without introducing
dependency on Aeson inside the enigne itself and keeps the concerns separated. Without them the 
instances would become orphan instances.

Keeping the protocol definitions separate preserves the boundary between the game logic and the
network communication. So the engine becomes free from knowledge of JSON and the server can
serialise engine types when communicating with clients.
-}

module Server.Protocol where


import Data.Aeson

import Engine.EngineTypes

----------------------------------------------------------------------------------------
-- This module defines all messages that travel between clients and the server.
-- It also contains the JSON serialisation instances for engine types (Action, GameEvent, Card, GamePhase)
-- so they can be sent.
-- We have it here because the engine has no dependency on Aeson. JSON is not an engine concern.
----------------------------------------------------------------------------------------

-- * Messages sent from the client

-- | Every message the client can send. Server decodes from JSON.
--
-- Incoming JSON messages are decoded into this type before being translated to 
-- server side state transitions. Each constructor corresponds to a distinct "type" field in the JSON.
data ClientMsg
    -- | Request to join the game: {"type":"join","name":"Alice"}
    = JoinMsg PlayerName

    -- | Request to leave the game: {"type":"leave"}
    | LeaveMsg

    -- | Submit action: {"type":"action","action":{...}}            
    | ActionMsg Action      
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


-- * Snapshot and player state

-- | Public representation of a player's state, safe to broadcast to all clients.
--
-- A summary of one players state, safe to broadcast to other clients using 'Snapshot' broadcasts.
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

-- * Messages sent from the server

-- | Every message the server can send to a client.
--
-- Each constructor maps to a distinct JSON message identified by a @"type"@ field.
data ServerMsg
    -- | Welcome message sent after succesfully joining.
    = Welcome String

    -- | Error message sent when a request can't be fulfilled.
    | ErrorMsg String

    -- | Broadcast when a player joins the table.
    | JoinedTable PlayerName

    -- | Broadcast when a player leaves the table.
    | LeftTable PlayerName

    -- | Collection of game events that occured as a result of an action or state change.
    | GameEventMsgs [GameEvent]

    -- | Broadcast when a player joins the lobby, with their position in the queue.
    | LobbyJoined PlayerName Int

    -- | Updated lobby queue.
    | LobbyUpdate [PlayerName] 

    -- | A players private hand.
    | PrivateHand [String]

    -- | Reveals all remaining hands at showdown.
    | ShowdownHands [(PlayerName, [String])]

    -- | Indicates whose turn it is and what actions they can take.
    | YourTurn
        { turnPlayer       :: PlayerName 
        , availableActions :: [String]
        , toCall           :: Chip
        , yourChips        :: Chip
        }

    -- | A full snapshot of the game state ('Table').               
    | Snapshot
        { snapPlayers :: [PlayerSnapshot]
        , snapPot     :: Chip
        , snapPhase   :: String
        , snapBoard   :: [String]
        , snapHighBet :: Bet
        , snapDealer  :: PlayerName
        , snapPlayerTurn :: PlayerName -- whose turn it is to act
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

    toJSON (PrivateHand cards) =
        object ["type"   .= ("private_hand" :: String)
               , "cards" .= cards]

    
    toJSON (YourTurn player actions call chip) =
        object [ "type"              .= ("your_turn" :: String)
               , "player"            .= player
               , "available_actions" .= actions
               , "to_call"           .= call
               , "your_chips"        .= chip
               ]

    toJSON (ShowdownHands hands) =
        object ["type" .= ("showdown_hands" :: String)
               , "hands" .= map (\(n, cards) -> object ["player" .= n, "cards" .= cards]) hands]

    toJSON s@(Snapshot{}) =
        object
            [ "type"     .= ("snapshot" :: String)
            , "players"  .= snapPlayers s
            , "pot"      .= snapPot s
            , "phase"    .= snapPhase s     -- ToJSON GamePhase below
            , "board"    .= snapBoard s     -- ToJSON Card below
            , "high_bet" .= snapHighBet s
            , "dealer"   .= snapDealer s
            , "player_turn" .= snapPlayerTurn s
            ]


----------------------------------------------------------------------------------------
--  SERIALISATION FOR ENGINE TYPES
--
--
-- These instances lets us send engine types over JSON without the engine knowing about JSON.
----------------------------------------------------------------------------------------

-- * Engine JSON wrappers

-- | Wrapper for 'Action' used to provide JSON instances without creating orphan instances on the engine.
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

-- | Wrapper around 'Card' used for JSON serialisation.
newtype CardJSON = CardJSON Card

-- | We encode a Card as a simple string which the show instance gives us.
instance ToJSON CardJSON where
    toJSON (CardJSON c) = toJSON (show c) 

-- | Wrapper around 'GamePhase' used for JSON serialisation.
newtype GamePhaseJSON = GamePhaseJSON GamePhase

-- | Same for GamePhase as Card
instance ToJSON GamePhaseJSON where
    toJSON (GamePhaseJSON p) = toJSON (show p)

-- | Wrapper around 'GameEvent' used for JSON serialisation.
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

    -- | When blinds are placed at the start of a hand, we want to broadcast who posted what.
    toJSON (GameEventJSON (PlayerPlacedBlinds n bt amt)) =
        object ["type"       .= ("blind"   :: String)
               , "player"    .= n
               , "blind_type".= show bt    -- SmallBlind or BigBlind
               , "amount"    .= amt
               ]

    -- | List of player names who are playing this hand
    toJSON (GameEventJSON (HandStarted players')) =
        object ["type" .= ("hand_started" :: String), "players" .= players']

    -- | Which phase the game advanced into: PreFlop, Flop, Turn or River.
    toJSON (GameEventJSON (PhaseChanged phase')) =
        object ["type" .= ("phase_changed" :: String), "phase" .= GamePhaseJSON phase']

    -- | ShowdownHappened contains a list of winners, because in case of a tie there can be multiple.
    toJSON (GameEventJSON (ShowdownHappened winners)) =
        object ["type"    .= ("showdown" :: String)
               , "winners" .= winners
               ]

    -- | List of players eliminated when a hand ends. Can be empty if no one was eliminated.
    toJSON (GameEventJSON (PlayerEliminated names)) =
        object ["type" .= ("eliminated" :: String), "players" .= names]

    -- | List of (name, amount) pairs for who won what in the showdown.
    toJSON (GameEventJSON (ChipsAwarded pair)) =
        object ["type" .= ("chips_awarded" :: String)
               , "awards" .= map (\(n, amt) -> object ["player" .= n, "amount" .= amt]) pair
               ]
             
           


    
