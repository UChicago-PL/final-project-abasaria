module Game.Types where

type Pos = (Int, Int)

data Cell = Cell
  { hasMine  :: Bool
  , revealed :: Bool
  , flagged  :: Bool
  , adjMines :: Int
  } deriving (Show)

type Board = [[Cell]]

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
