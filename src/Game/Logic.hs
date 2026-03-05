module Game.Logic
  ( revealCell
  , flagCell
  ) where

import Game.Types
import Game.Board

-- reveals a cell and updates the game state accordingly
revealCell :: Pos -> GameState -> GameState
revealCell pos gs@(GameState b currentStatus)
  | currentStatus /= Playing = gs
  | not (inBounds b pos) = gs
  | revealed cell || flagged cell = gs
  | hasMine cell =
      GameState (revealAllMines b) Lost
  | adjMines cell == 0 =
      finalize (floodFill [pos] b)
  | otherwise =
      finalize (updateBoard b pos (\c -> c { revealed = True }))
  where
    cell = b !! fst pos !! snd pos

-- reveals all mines on the board (used on loss)
revealAllMines :: Board -> Board
revealAllMines =
  map (map revealMine)
  where
    revealMine c
      | hasMine c = c { revealed = True }
      | otherwise = c

-- checks if all non-mine cells are revealed to see if the player has won
finalize :: Board -> GameState
finalize newBoard =
  if allSafeRevealed newBoard
     then GameState newBoard Won
     else GameState newBoard Playing

-- checks if all non-mine cells are revealed(helper for above function)
allSafeRevealed :: Board -> Bool
allSafeRevealed b =
  all (all (\c -> hasMine c || revealed c)) b

-- performs a flood fill to reveal all connected empty cells and their neighbors
floodFill :: [Pos] -> Board -> Board
floodFill [] b = b
floodFill (p:ps) b
  | not (inBounds b p) = floodFill ps b
  | revealed cell || flagged cell = floodFill ps b
  | hasMine cell = floodFill ps b
  | adjMines cell == 0 =
      floodFill (ps ++ neighbors p b) b'
  | otherwise =
      floodFill ps b'
  where
    cell = b !! fst p !! snd p
    b' = updateBoard b p (\c -> c { revealed = True })

-- flagged state functionality
flagCell :: Pos -> GameState -> GameState
flagCell pos gs@(GameState b currentStatus)
  | currentStatus /= Playing = gs
  | not (inBounds b pos) = gs
  | revealed cell = gs
  | otherwise =
      GameState
        (updateBoard b pos (\c -> c { flagged = not (flagged c) }))
        currentStatus
  where
    cell = b !! fst pos !! snd pos

-- gets valid neighboring positions for flood fill
neighbors :: Pos -> Board -> [Pos]
neighbors (r, c) b =
  [ (r + dr, c + dc)
  | dr <- [-1..1]
  , dc <- [-1..1]
  , (dr, dc) /= (0,0)
  , inBounds b (r + dr, c + dc)
  ]
