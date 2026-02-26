module Game.Types where

type Pos = (Int, Int)

data Cell = Cell
  { hasMine  :: Bool
  , revealed :: Bool
  , flagged  :: Bool
  , adjMines :: Int
  } deriving (Show, Eq)

type Board = [[Cell]]

data GameStatus = Playing | Lost | Won
  deriving (Show, Eq)

data GameState = GameState
  { board  :: Board
  , status :: GameStatus
  } deriving (Show, Eq)
