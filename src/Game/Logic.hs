module Game.Logic
  ( revealCell
  , flagCell
  ) where

import Game.Types
import Game.Board


revealCell :: Pos -> GameState -> GameState
revealCell pos gs@(GameState b currentStatus)
  | currentStatus /= Playing = gs
  | not (inBounds b pos) = gs
  | revealed cell || flagged cell = gs
  | hasMine cell = GameState (updateReveal b) Lost
  | adjMines cell == 0 =
      finalize (floodFill [pos] b)
  | otherwise =
      finalize (updateBoard b pos (\c -> c { revealed = True }))
  where
    cell = b !! fst pos !! snd pos
    updateReveal brd = updateBoard brd pos (\c -> c { revealed = True })


finalize :: Board -> GameState
finalize newBoard =
  if allSafeRevealed newBoard
     then GameState newBoard Won
     else GameState newBoard Playing

allSafeRevealed :: Board -> Bool
allSafeRevealed b =
  all (all (\c -> hasMine c || revealed c)) b


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


neighbors :: Pos -> Board -> [Pos]
neighbors (r, c) b =
  [ (r + dr, c + dc)
  | dr <- [-1..1]
  , dc <- [-1..1]
  , (dr, dc) /= (0,0)
  , inBounds b (r + dr, c + dc)
  ]
