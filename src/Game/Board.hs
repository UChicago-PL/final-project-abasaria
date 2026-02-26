module Game.Board
  ( initBoard
  , updateBoard
  , inBounds
  ) where

import Game.Types
import System.Random (randomRIO)

-- | Initialize a new board with given rows, cols, and mine count
initBoard :: Int -> Int -> Int -> IO Board
initBoard rows cols mineCount = do
  minePositions <- generateMines rows cols mineCount
  let empty = emptyBoard rows cols
      withMines = placeMines empty minePositions
  return (computeAdjacencies withMines)

-- | Create an empty board (no mines yet)
emptyBoard :: Int -> Int -> Board
emptyBoard rows cols =
  replicate rows (replicate cols emptyCell)

-- | A default empty cell
emptyCell :: Cell
emptyCell = Cell
  { hasMine  = False
  , revealed = False
  , flagged  = False
  , adjMines = 0
  }

-- | Randomly generate unique mine positions
generateMines :: Int -> Int -> Int -> IO [Pos]
generateMines rows cols count = go []
  where
    go acc
      | length acc == count = return acc
      | otherwise = do
          r <- randomRIO (0, rows - 1)
          c <- randomRIO (0, cols - 1)
          let pos = (r, c)
          if pos `elem` acc
            then go acc
            else go (pos : acc)

-- | Place mines at specified positions
placeMines :: Board -> [Pos] -> Board
placeMines = foldl placeMine

placeMine :: Board -> Pos -> Board
placeMine b (r, c) =
  updateBoard b (r, c) (\cell -> cell { hasMine = True })

-- | Update a specific position on the board
updateBoard :: Board -> Pos -> (Cell -> Cell) -> Board
updateBoard b (r, c) f =
  take r b
  ++ [updateRow (b !! r)]
  ++ drop (r + 1) b
  where
    updateRow row =
      take c row
      ++ [f (row !! c)]
      ++ drop (c + 1) row

-- | Compute adjacent mine counts for all cells
computeAdjacencies :: Board -> Board
computeAdjacencies b =
  [ [ updateCell r c | c <- [0 .. cols - 1] ]
  | r <- [0 .. rows - 1]
  ]
  where
    rows = length b
    cols = length (head b)

    updateCell r c =
      let cell = b !! r !! c
      in if hasMine cell
           then cell
           else cell { adjMines = countAdjacentMines b (r, c) }

-- | Count mines adjacent to a position
countAdjacentMines :: Board -> Pos -> Int
countAdjacentMines b (r, c) =
  length
    [ ()
    | (dr, dc) <- deltas
    , let nr = r + dr
    , let nc = c + dc
    , inBounds b (nr, nc)
    , hasMine (b !! nr !! nc)
    ]
  where
    deltas =
      [ (-1, -1), (-1, 0), (-1, 1)
      , (0, -1),           (0, 1)
      , (1, -1),  (1, 0),  (1, 1)
      ]

-- | Check if a position is inside the board
inBounds :: Board -> Pos -> Bool
inBounds b (r, c) =
  r >= 0 && r < length b &&
  c >= 0 && c < length (head b)
