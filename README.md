# Haskell Hold'em
A multiplayer Texas Hold'em poker server written in Haskell. The project features a pure functional game engine, a WebSocket server for real-time multiplayer, and a browser-based client for playing the game across multiple tabs or machines on localhost.

Built as part of the Functional Programming Project course (DIT216).

## Table of Contents

- [Project Structure](#project-structure)
- [Installation](#installation)
- [Usage](#usage)
- [Testing](#testing)
- [What's next?](#whats-next)
- [Contributors](#contributors)
- [License](#license)

---

## Project Structure

The project is structured into three main parts: the game engine, the server layer, and the test suite.

The `src/Engine` module contains the core game logic. This is where card representations, hand evaluation, game rules, and utility functions live. It's the part that determines outcomes of the game state and player actions using stateful computations.

The `src/Server` module handles everything related to communication and runtime execution. It manages the game loop, networking, and the protocol used to send and receive JSON messages between the client and server.

The `test` folder contains unit and property tests that validate stateful computations and hand evaluation logic to ensure core behaviour acts as expected.

```bash
.
│   Haskell-hold-em.cabal
│   poker-test-client.html
│
├───app
│       Main.hs
│
├───src
│   ├───Engine
│   │       Actions.hs
│   │       Cards.hs
│   │       EngineTypes.hs
│   │       HandEvaluation.hs
│   │       TexasEngine.hs
│   │       Utilities.hs
│   │
│   └───Server
│           GameLoop.hs
│           NetworkServer.hs
│           Protocol.hs
│           ServerHelpers.hs
│           ServerTypes.hs
│
└───test
        Main.hs
        StateTest.hs
        TestHandEvaluation.hs
        TestHelpers.hs
        UtilitiesTest.hs
```

### Architecture Diagram

This diagram visualises the internal structure of the application and the relationships between its modules. It illustrates how the different components interact and how control flows through function calls. In particular, it highlights how JSON is sent between the client and server to keep the game state in sync with client-side rendering.

<details>
  <summary>Flowchart diagram</summary>
  <img width="929" height="1916" alt="flowchart2 drawio" src="https://github.com/user-attachments/assets/bf056e12-28c2-42c2-b64e-c0eacb4d8399" />
</details>

---

## Installation

### Prerequisites

- [GHC](https://www.haskell.org/ghc/) 9.6.x
- [Cabal](https://cabal.readthedocs.io/en/stable/) >=3.0

The easiest way to get both is with [GHCup](https://www.haskell.org/ghcup/) to install the entire Haskell toolchain.

### Dependencies

All dependencies are declared in `Haskell-hold-em.cabal` and fecthed automatically by Cabal. Some key libraryies used are:

| Library | Purpose |
|---------|---------|
| `websockets`| WebSocket server for client connections |
| `aeson` | JSON endocoding and decoding |
| `stm` | Software Transactional Memory for concurrent state |
| `random` | Deck shuffling |
| `containers`| `Map` for client lookup |

### Build

Clone the repository and build with Cabal:

```bash
git clone https://github.com/oggeboggee/Haskell_hold-em.git
cd Haskell_hold-em
cabal build
```

---

## Usage

### Running the server

```bash
cabal run
```

The server will start on `ws://localhost:9160` and print a confirmation in the terminal:

```
Poker server running on ws://localhost:9160
```

### Playing the game
Open `poker-client.html` in two separate browser tabs, each tab is now its own client and represents one player.

1. Click **Connect** to connect to the server
2. Enter a name and click **Join**
3. Once two or more players have joined, the hand starts automatically
4. Players take turns acting, the banner shows whose turn it is and what actions are available.
5. After the hand ends, winner(s) will be displayed and the game state will update to reflect winners/losers.
6. A new hand starts

The client displays:
* The current game phase (PreFlop, Flop, Turn, River, Showdown)
* The players hand (visible only to you)
* The community cards as they are delt
* Which player is the dealer
* A timestamped game log of all messages being sent
* A log with "pretty-printed" actions happening during the game

--- 

## Testing
To run the test suite:

```bash
cabal test
```

The test cover:
* **Hand evaluation** - to verify that the handEvaluation module correctly identifies all hand combinations, including edge cases like ace-low straights.
*  **Stateful computations** - 

---

## What's next?

- [ ] Expand the testing suite to include more extensive testing
- [ ] Replace static html client with a proper frontend application using some rendering framework (e.g Miso)
- [ ] Migrate from a local server setup to a remote server environment

---

## Contributors
- [![GitHub](https://img.shields.io/badge/GitHub-oggeboggee-181717?logo=github)](https://github.com/oggeboggee)
- [![GitHub](https://img.shields.io/badge/GitHub-linuxcodes-181717?logo=github)](https://github.com/linuxcodes)
- [![GitHub](https://img.shields.io/badge/GitHub-AxelNygren-181717?logo=github)](https://github.com/AxelNygren)

---

## License

See [license](https://github.com/oggeboggee/Haskell_hold-em?tab=BSD-3-Clause-1-ov-file)
