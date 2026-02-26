module Game.GUI
  ( draw
  , handleMouse
  , update
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Game.Types
import Game.Board
import Game.Logic

-- GUI constants
cellSize :: Float
cellSize = 40

window :: Display
window = InWindow "Minesweeper" (600, 600) (100, 100)

backgroundColor :: Color
backgroundColor = greyN 0.7

-- | Convert board coordinates to screen coordinates
toScreen :: Pos -> (Float, Float)
toScreen (r, c) = (fromIntegral c * cellSize - 200, 200 - fromIntegral r * cellSize)

-- | Draw the entire board
draw :: GameState -> IO Picture
draw (GameState b _) = return $ Pictures [ drawCellAt r c (b !! r !! c)
                                         | r <- [0..length b - 1]
                                         , c <- [0..length (head b) - 1] ]

-- | Draw a single cell at a given board position
drawCellAt :: Int -> Int -> Cell -> Picture
drawCellAt r c cell =
  translate x y $ drawCell cell
  where
    (x, y) = toScreen (r, c)

drawCell :: Cell -> Picture
drawCell c
  | flagged c        = color red (rectangleSolid cellSize cellSize)
  | not (revealed c) = color (greyN 0.5) (rectangleSolid cellSize cellSize)
  | hasMine c        = color black (rectangleSolid cellSize cellSize)
  | adjMines c == 0  = color white (rectangleSolid cellSize cellSize)
  | otherwise        =
      Pictures
        [ color white (rectangleSolid cellSize cellSize)
        , color black (translate (-cellSize/4) (-cellSize/4)
            (Scale 0.2 0.2 (Text (show (adjMines c)))))
        ]

-- | Handle mouse clicks
handleMouse :: Event -> GameState -> IO GameState
handleMouse (EventKey (MouseButton LeftButton) Up _ (mx, my)) gs = do
  let pos = toBoard (mx, my)
  return $ revealCell pos gs
handleMouse (EventKey (MouseButton RightButton) Up _ (mx, my)) gs = do
  let pos = toBoard (mx, my)
  return $ flagCell pos gs
handleMouse _ gs = return gs

-- | Convert screen coordinates back to board coordinates
toBoard :: (Float, Float) -> Pos
toBoard (x, y) = (row, col)
  where
    col = floor ((x + 200) / cellSize)
    row = floor ((200 - y) / cellSize)

-- | Update function (no-op for now)
update :: Float -> GameState -> IO GameState
update _ gs = return gs
