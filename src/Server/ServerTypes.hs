{-|
Module      : Server.ServerTypes
Description : Core server-side types and shared definitions.

Data definitions and modeling shared between 'Server.NetworkServer' and 'Server.GameLoop'.
Server keeps track of two things at the same time:

[@Table@] - The poker game state.

[@Clients@] - The connected websocket clients

Both of these live together in ServerState and is shared across all client threads.

-}

module Server.ServerTypes where

import Engine.EngineTypes
import qualified Network.WebSockets as WS
import qualified Data.Map.Strict    as M
import           Data.Map           (Map)
import           Control.Concurrent.STM.TVar (TVar)
import           Server.Protocol    (ServerMsg(..))

-- | Unique ID assigned to each client connection when it connects.
--   Lets us track clients independently of whether they've chosen a name
--   or not. Gets assigned atomically so no clients risk sharing an ID.
type ClientId = Int

-- | Information associated with a connected client.
--   A client may be connected before joining the game,
--   in that case 'clientName' is empty until a 'JoinMsg' is received.
data ClientInfo = ClientInfo
    { clientName :: PlayerName          -- Chosen name of the player
    , connection :: WS.Connection       -- A live websocket connection to their client.
    }

-- | Map of: ClientID -> ClientInfo. Lookup structure for conected clients.
--   E.g:
--
-- @
-- 0 -> ("Sam", conn1)
-- 1 -> ("Bob", conn2)
-- 2 -> (""   , conn3) <- connected but not yet joined.
-- @
--
--   We need this to reach specific clients and find out what name belongs to what client.
--   This helps us to send messages to specific clients or broadcast to everyone.
type Clients = Map ClientId ClientInfo

-- | The complete server state.
--
--   Contains the current game state ('Table'), the connected 'Clients', a counter to generate
--   unique 'ClientId's, the lobby of waiting players and whether a hand is ongoing or not.
data ServerState = ServerState 
    { table        :: Table         -- poker game state (engine)
    , clients      :: Clients       -- all connected websocket clients
    , nextClientId :: ClientId      -- counter to generate unique cIDs.
    , lobby        :: [PlayerName]  -- ordered queue of players waiting to join the game
    , handOngoing  :: Bool          -- Is hand ongoing or not.
    }

-- | A shared mutable state across all websocket threads.
--   Each connected client gets it's own thread, so the state must be shared safely.
--   TVar from STM gives us atomic reads and writes to avoid the risk of corruption.
--   Meaning if two clients were to act simultaneously then one will succeed and the other retry.
type SharedServerState = TVar ServerState


-- | The initial server state, built from an initial table.
--   No connected clients at this stage, an empty lobby and ID counter starting at 0.
initServerState :: Table -> ServerState
initServerState t = 
    ServerState
        { table        = t          -- Engine owned initial table
        , clients      = M.empty    -- Websocket connections (none so far)
        , nextClientId = 0          -- Client identifier (First client gets assigned an ID of 0)
        , lobby        = []         -- nobody waiting to join yet
        , handOngoing  = False      -- No hand ongoing at initial state
        }


-- | A state chaning event expressed as a Transition.
--
-- A transition described what just happened from the network layers perspective.
-- 'Server.NetworkServer' creates transitions from 'ClientMsg's and passes them to 'Server.GameLoop'.
-- 'Server.GameLoop' then pattern matches on Transition variants to decide what to do next.
--
-- This decouples the network layer which uses 'ClientMsg' from the game layer which uses Transition.
-- 'Server.NetworkServer' only needs to know how to translate, 'Server.GameLoop' only needs to know how to transition.
data Transition
    -- | A client sent E.g {"type":"join","name":"Alice"}.
    = PlayerJoined ClientId PlayerName

    -- | A client explicitly sent {"type":"leave"}
    -- ClientId is enough as GameLoop looks up their name internally.
    | PlayerLeft ClientId
    
    -- | A seated player sent an action message like {"type":"action","action":"fold"}
    -- ClientId will be resolve to a PlayerIndex before the engine sees it.
    | PlayerActed ClientId Action
    deriving (Show)

------------------------------------------------------------------------------------------------
-- BroadcastAction TYPE
--
-- GameLoop decides what messages should be sent and to who, but does not send them directly.
-- Instead it returns a BroadcastAction to NetworkServer which then executes the sending.
-- This decouples GameLoop from websocket details, i.e it never calls WS.sendTextData or touches
-- WS.Connection directly.

-- NetworkServer receives the list and executes each action in order, looking up connections
-- from the clients map as needed. 
------------------------------------------------------------------------------------------------

-- | Describes a message to be sent, and who should receive it.
--   Returned by 'Servre.GameLoop' functions, executed by 'Server.NetworkServer'.
data BroadcastAction
    -- | Send the message to all connected clients.
    -- Used for public state changes such as snapshots, game events, lobby updates etc.
    = BroadcastToAll ServerMsg 

    -- | Send the message to one specific client only.
    -- USed for private messages: errors, lobby position, hands, YourTurn etc.
    | SendToClient ClientId ServerMsg

    -- | Send this message to all clients except one.
    -- USed when the excluded client already received the same information in a private message,
    -- e.g when a player acts.
    | BroadcastToAllExcept ClientId ServerMsg 
    deriving (Show)