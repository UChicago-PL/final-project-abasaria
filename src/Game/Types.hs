module Game.Types where

type Pos = (Int, Int)

-- represents a single cell on the board, with its properties
data Cell = Cell
  { hasMine  :: Bool
  , revealed :: Bool
  , flagged  :: Bool
  , adjMines :: Int
  } deriving (Show)

type Board = [[Cell]]

-- represents the overall game state, including the board and current status
data GameStatus
  = Menu
  | Playing
  | Won
  | Lost
  deriving (Eq, Show)

data GameState = GameState
  { board  :: Board
  , status :: GameStatus
  } deriving (Show)
