module Game.Logic
  ( revealCell
  , flagCell
  , checkWin
  ) where

import Game.Types
import Game.Board

-- | Reveal a cell at a given position
revealCell :: Pos -> GameState -> GameState
revealCell pos gs@(GameState b currentStatus)
  | currentStatus /= Playing = gs
  | not (inBounds b pos) = gs
  | revealed cell || flagged cell = gs
  | hasMine cell = GameState b' Lost
  | adjMines cell == 0 = GameState (floodFill [pos] b) Playing
  | otherwise = GameState (updateBoard b pos (\c -> c { revealed = True })) Playing
  where
    cell = b !! fst pos !! snd pos
    b' = updateBoard b pos (\c -> c { revealed = True })

-- | Flood-fill revealing for empty cells
floodFill :: [Pos] -> Board -> Board
floodFill [] b = b
floodFill (p:ps) b
  | not (inBounds b p) = floodFill ps b
  | revealed cell || flagged cell = floodFill ps b
  | hasMine cell = floodFill ps b
  | adjMines cell == 0 = floodFill (ps ++ neighbors p b) b'
  | otherwise = floodFill ps b'
  where
    cell = b !! fst p !! snd p
    b' = updateBoard b p (\c -> c { revealed = True })

-- | Toggle flag on a hidden cell
flagCell :: Pos -> GameState -> GameState
flagCell pos gs@(GameState b currentStatus)
  | currentStatus /= Playing = gs
  | not (inBounds b pos) = gs
  | revealed cell = gs
  | otherwise = GameState (updateBoard b pos (\c -> c { flagged = not (flagged c) })) currentStatus
  where
    cell = b !! fst pos !! snd pos

-- | Check if all non-mine cells are revealed
checkWin :: GameState -> Bool
checkWin (GameState b _) =
  all rowComplete b
  where
    rowComplete = all cellComplete
    cellComplete c = revealed c || hasMine c

-- | Get neighboring positions (8 directions)
neighbors :: Pos -> Board -> [Pos]
neighbors (r, c) b =
  [ (r + dr, c + dc)
  | dr <- [-1..1]
  , dc <- [-1..1]
  , (dr, dc) /= (0,0)
  , inBounds b (r + dr, c + dc)
  ]
