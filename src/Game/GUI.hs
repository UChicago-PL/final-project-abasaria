module Game.GUI where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Game.Types
import Game.Board
import Game.Logic

-- | Size of each cell in pixels
cellSize :: Float
cellSize = 30

-- | Convert board coordinates to screen coordinates
toScreen :: Pos -> (Float, Float)
toScreen (r, c) =
  (fromIntegral c * cellSize - offsetX, offsetY - fromIntegral r * cellSize)
  where
    offsetX = 5 * cellSize  -- for a 10x10 board
    offsetY = 5 * cellSize

-- | Draw the entire board
drawBoard :: Board -> Picture
drawBoard b = Pictures [translate x y (drawCell cell) | (r, row) <- zip [0..] b
                                                       , (c, cell) <- zip [0..] row
                                                       , let (x, y) = toScreen (r, c)]

-- | Draw a single cell
drawCell :: Cell -> Picture
drawCell c
  | flagged c = Pictures
      [ color red (rectangleSolid cellSize cellSize)
      , color black (translate (-cellSize/4) (-cellSize/4)
                 (Scale 0.5 0.5 (Text "F")))
      ]
  | not (revealed c) = color (greyN 0.5) (rectangleSolid cellSize cellSize)
  | hasMine c = color black (rectangleSolid cellSize cellSize)
  | adjMines c == 0 = color white (rectangleSolid cellSize cellSize)
  | otherwise = Pictures
      [ color white (rectangleSolid cellSize cellSize)
      , color black (translate (-cellSize/4) (-cellSize/4)
                 (Scale 0.2 0.2 (Text (show (adjMines c)))))
      ]

-- | Handle mouse clicks
handleMouse :: Event -> GameState -> GameState
handleMouse (EventKey (MouseButton LeftButton) Down _ (mx, my)) gs =
  let pos = toBoard (mx, my)
  in revealCell pos gs
handleMouse (EventKey (MouseButton RightButton) Down _ (mx, my)) gs =
  let pos = toBoard (mx, my)
  in flagCell pos gs
handleMouse _ gs = gs

-- | Convert screen coordinates to board coordinates
toBoard :: (Float, Float) -> Pos
toBoard (x, y) =
  (r, c)
  where
    offsetX = 5 * cellSize
    offsetY = 5 * cellSize
    c = round ((x + offsetX) / cellSize)
    r = round ((offsetY - y) / cellSize)

-- | Update function (required by Gloss, we do nothing for now)
update :: Float -> GameState -> GameState
update _ gs = gs

-- | Draw GameState
draw :: GameState -> Picture
draw (GameState b _) = drawBoard b
