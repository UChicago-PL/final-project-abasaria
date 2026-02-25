module Game.Types
  ( Pos
  , Cell(..)
  , Board
  , Status(..)
  , GameState(..)
  ) where

-- A position on the board (row, column)
type Pos = (Int, Int)

-- A single cell on the board
data Cell = Cell
  { hasMine  :: Bool      -- True if this cell contains a mine
  , revealed :: Bool      -- True if the player has revealed this cell
  , flagged  :: Bool      -- True if the player has flagged this cell
  , adjMines :: Int       -- Number of adjacent mines
  }
  deriving (Eq, Show)

-- The game board is a 2D grid of cells
type Board = [[Cell]]

-- Current status of the game
data Status
  = Playing
  | Won
  | Lost
  deriving (Eq, Show)

-- Overall game state
data GameState = GameState
  { board  :: Board
  , status :: Status
  }
  deriving (Eq, Show)
