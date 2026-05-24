module Server.ServerHelpers
    ( maxSeats
    , clientExists
    , lookupClient
    , lookupPlayer
    , findPlayerIndex
    , resolvePIdx
    , nameClient
    , removeClient
    , isInLobby
    , isSeated
    , numSeatedPlayers
    , isTableFull
    , findPlayerByName
    , addPlayerToLobby
    , removePlayerFromLobby
    , returnPlayersToLobby
    , seatLobbyPlayersAtTable
    ) where

import Server.ServerTypes
import Engine.EngineTypes
import qualified Data.Map.Strict as M
import Data.List (findIndex, find)
import Engine.TexasEngine (addPlayer)

-- | Maximum number of players allowed at the table at once.
--   Players that try to join beyond thix limit will be put in the lobby queue.
maxSeats :: Int
maxSeats = 6

----------------------------------------------------------------------------------------
-- CLIENT LOOKUP HELPERS
--
-- Pure functions to lookup Serverstate without modifying it.
-- Bridge between WS ClientIDs and poker player identity (name and index at the table)
-- Engine uses PlayerIndex (position in the players list)
-- Server uses ClientID (WS connection identifier assigned on connection)
-- With these functions we can translate between both.
----------------------------------------------------------------------------------------

-- | Check if a client with the given name already exists in the server state.
clientExists :: PlayerName -> ServerState -> Bool
clientExists n st = any ((==n) . clientName) (M.elems (clients st))

-- | Lookup the ClientInfo for a given ClientId. Returns Nothing if not found.
--   (Should not happen since the IDs are allocated on connection)
lookupClient :: ClientId -> ServerState -> Maybe ClientInfo
lookupClient cid st = M.lookup cid (clients st)

-- | Lookup the player name associated with a given ClientId. Returns Nothing if the
--   client hasn't sent a JoinMsg yet (i.e. they are connected but not yet seated at the table
--   and their name field is still empty from the initial ClientInfo).
lookupPlayer :: ClientId -> ServerState -> Maybe PlayerName
lookupPlayer cid st = do
    info <- lookupClient cid st
    pure (clientName info)

-- | Find a players index in the engine's players list by their name.
--   Engine identifies players by their index in the players list (i.e PlayerIndex). So
--   this translates the players name into the index that the engine expects.
findPlayerIndex :: PlayerName -> Table -> Maybe PlayerIndex
findPlayerIndex n t = findIndex (\p -> name p == n) (players t)

-- | Resolve a ClientID to a PlayerIndex by looking up the client, then their name,
--   and then finding the index of that name in the engine's players list.
--   This is used before every call to the engine. Returns Nothing if the client
--   isn't seated (in the lobby, haven't joined, or their name doesn't exist in the Table).
resolvePIdx :: ServerState -> ClientId -> Maybe PlayerIndex
resolvePIdx st cid = do
    n <- lookupPlayer cid st
    findPlayerIndex n (table st)

----------------------------------------------------------------------------------------
-- CLIENT STATE MODIFIERS
--
-- Functions to modify the ServerState and return a new ServerState. With the modifications applied.
-- Callers are responsible for writing the new state back into the TVar with STM.
----------------------------------------------------------------------------------------

-- | Assign a player name to an already connected client. This is called when a client
--   sends a JoinMsg with their chosen name. The client already exists as it was created
--   on connection, but their name field is empty until they send the JoinMsg and update
--   their name with this function.
nameClient :: ClientId -> PlayerName -> ServerState -> ServerState
nameClient cid playerName st =
    case lookupClient cid st of
        Nothing -> st
        Just info -> st { clients = M.insert cid (info { clientName = playerName }) (clients st) }

-- | Remove a client from the server state using their ClientId.
--   This is called when a client leaves the server or disconnects.
--   It does not remove them from the Tables players list, that is handled separately
--   by the engines removePlayer function.
removeClient :: ClientId -> ServerState -> ServerState
removeClient cid st = st { clients = M.delete cid (clients st) }

----------------------------------------------------------------------------------------
-- LOBBY HELPERS
--
-- The lobby is the queue of players waiting to be seated at the table. Players join the lobby
-- first regardless of whether there is space at the table or not. Seats are only filled at
-- the start of a hand. All seated players return to the front of the lobby at the end of a hand
-- so they get priority over the new joiners and waiting players.
----------------------------------------------------------------------------------------

-- | Checks if a player is currently in the lobby queue.
isInLobby :: PlayerName -> ServerState -> Bool
isInLobby n st = n `elem` lobby st

-- | Check if a player with the given name is currently seated at the table.
--   (Seated means they are in the engines players list.)
isSeated :: PlayerName -> ServerState -> Bool
isSeated n st = any ((==n) . name) (players (table st))

-- | Get the number of players currently seated at the table.
numSeatedPlayers :: ServerState -> Int
numSeatedPlayers st = length (players (table st))

-- | Check if the table has reached its maximum capacity.
isTableFull :: ServerState -> Bool
isTableFull st = length (players (table st)) >= maxSeats

-- | Find a seated player by their name. Returns Nothing if the player is not currently seated.
findPlayerByName :: PlayerName -> Table -> Maybe Player
findPlayerByName n t = find (\p -> name p == n) (players t)

-- | Add a player to the lobby queue.
--   Players get appended to the end so the queue is FIFO.
addPlayerToLobby :: PlayerName -> ServerState -> ServerState
addPlayerToLobby plName st = st { lobby = lobby st ++ [plName] }

-- | Remove a player from the lobby queue.
--   Called when a player leaves while waiting, or when they get seated at the table
--   at the start of a hand.
removePlayerFromLobby :: PlayerName -> ServerState -> ServerState
removePlayerFromLobby plName st = st { lobby = filter (/= plName) (lobby st) }

-- | After a hand ends we move all seated players back to the front of the lobby queue.
--   We do this to ensure the table is fully cleared between hands and previously seated
--   players get priority over new joiners and waiting players.
returnPlayersToLobby :: ServerState -> ServerState
returnPlayersToLobby st =
    let seatedPlNames = map name (players (table st))
        waitingNames  = lobby st
        updatedLobby  = seatedPlNames ++ waitingNames
        updatedTable  = (table st) { players = [] }
    in st { lobby = updatedLobby, table = updatedTable }

-- | Fill seats from the front of the lobby queue at the start of a hand up to the maxSeats limit.
--   Players get seated from left to right in the order they appear in the queue, and any
--   players beyond the maxSeats remain in the lobby queue.

-- | The helper function seatThePlayers recursively seats a list of players at the table.
--   Each player gets added to the end of the players list via addPlayer from the engine.
--   seatLobbyPlayersAtTable calls it with the subset of lobby players to seat.
seatLobbyPlayersAtTable :: ServerState -> ServerState
seatLobbyPlayersAtTable st =
    let playersToSeat = take maxSeats (lobby st)
        remianingLobby = drop maxSeats (lobby st)
        updatedTable = seatThePlayers playersToSeat (table st)
    in st { lobby = remianingLobby, table = updatedTable }
    where
        seatThePlayers :: [PlayerName] -> Table -> Table
        seatThePlayers [] t = t
        seatThePlayers (p:ps) t = seatThePlayers ps (addPlayer p t)

