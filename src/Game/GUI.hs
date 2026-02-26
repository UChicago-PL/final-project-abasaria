module Game.GUI
  ( draw
  , handleMouse
  , update
  , cellSize
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Game.Types
import Game.Logic

-- | Size of each cell in pixels
cellSize :: Float
cellSize = 40

-- | Convert board coordinates to screen coordinates
toScreen :: Pos -> (Float, Float)
toScreen (r, c) =
  (fromIntegral c * cellSize - fromIntegral cols * cellSize / 2 + cellSize/2,
   fromIntegral (rows - r - 1) * cellSize - fromIntegral rows * cellSize / 2 + cellSize/2)
  where
    rows = 10  -- default board size; adjust if making dynamic
    cols = 10

-- | Draw a single cell
drawCell :: Cell -> Picture
drawCell c
  | flagged c = color red (rectangleSolid cellSize cellSize)
  | not (revealed c) = color (greyN 0.5) (rectangleSolid cellSize cellSize)
  | hasMine c = color black (rectangleSolid cellSize cellSize)
  | adjMines c == 0 = color white (rectangleSolid cellSize cellSize)
  | otherwise = Pictures
      [ color white (rectangleSolid cellSize cellSize)
      , color black (translate (-cellSize/4) (-cellSize/4)
                  (Scale 0.2 0.2 (Text (show (adjMines c)))))
      ]

-- | Draw the entire board
draw :: GameState -> IO Picture
draw gs = return $ Pictures
  [ translate x y (Pictures [drawCell cell, drawBorder])
  | (r, row) <- zip [0..] (board gs)
  , (c, cell) <- zip [0..] row
  , let (x, y) = toScreen (r, c)
  , let drawBorder = color black (rectangleWire cellSize cellSize)
  ]

-- | Handle mouse clicks
handleMouse :: Event -> GameState -> IO GameState
handleMouse (EventKey (MouseButton LeftButton) Down _ (mx, my)) gs =
  let pos = screenToPos (mx, my)
  in return $ revealCell pos gs
handleMouse (EventKey (MouseButton RightButton) Down _ (mx, my)) gs =
  let pos = screenToPos (mx, my)
  in return $ flagCell pos gs
handleMouse _ gs = return gs

-- | Convert screen coordinates back to board coordinates
screenToPos :: (Float, Float) -> Pos
screenToPos (x, y) =
  (rows - 1 - r, c)
  where
    r = floor ((y + fromIntegral rows * cellSize / 2) / cellSize)
    c = floor ((x + fromIntegral cols * cellSize / 2) / cellSize)
    rows = 10
    cols = 10

-- | Update function (no time-based updates needed)
update :: Float -> GameState -> IO GameState
update _ gs = return gs
