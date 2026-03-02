module Game.GUI
  ( draw
  , handleEvent
  , update
  ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Game.Types
import Game.Board
import Game.Logic
import Text.Read (readMaybe)

cellSize :: Float
cellSize = 35

draw :: GameState -> IO Picture
draw gs =
  return $
    case status gs of
      Menu -> drawMenu
      _    -> Pictures [drawBoard gs, drawUI gs]


drawMenu :: Picture
drawMenu =
  Pictures
    [ translate (-200) 180
        (Scale 0.3 0.3 (Text "MINESWEEPER"))
    , translate (-250) 80
        (Scale 0.15 0.15 (Text "Press 1 for Easy (8x8)"))
    , translate (-250) 40
        (Scale 0.15 0.15 (Text "Press 2 for Medium (12x12)"))
    , translate (-250) 0
        (Scale 0.15 0.15 (Text "Press 3 for Hard (16x16)"))
    , translate (-250) (-40)
        (Scale 0.15 0.15 (Text "Press 4 for Custom Game"))
    , translate (-250) (-100)
        (Scale 0.12 0.12 (Text "Custom game input will appear in terminal"))
    ]


drawBoard :: GameState -> Picture
drawBoard gs =
  Pictures
    [ translate x y (Pictures [drawCell cell, border])
    | (r, row) <- zip [0..] (board gs)
    , (c, cell) <- zip [0..] row
    , let x = fromIntegral c * cellSize - fromIntegral (cols gs) * cellSize / 2 + cellSize/2
    , let y = fromIntegral (rows gs - r - 1) * cellSize - fromIntegral (rows gs) * cellSize / 2 + cellSize/2
    , let border = color black (rectangleWire cellSize cellSize)
    ]

drawCell :: Cell -> Picture
drawCell c
  | flagged c = color red (rectangleSolid cellSize cellSize)
  | not (revealed c) = color (greyN 0.5) (rectangleSolid cellSize cellSize)
  | hasMine c = color black (rectangleSolid cellSize cellSize)
  | adjMines c == 0 = color white (rectangleSolid cellSize cellSize)
  | otherwise =
      Pictures
        [ color white (rectangleSolid cellSize cellSize)
        , color black
            (translate (-cellSize/4) (-cellSize/4)
              (Scale 0.2 0.2 (Text (show (adjMines c)))))
        ]


drawUI :: GameState -> Picture
drawUI gs =
  let safeRevealed =
        length
          [ ()
          | row <- board gs
          , cell <- row
          , revealed cell
          , not (hasMine cell)
          ]

      statusText =
        case status gs of
          Playing -> ""
          Won     -> "You Won! (Press R for Menu)"
          Lost    -> "Game Over (Press R for Menu)"
          Menu    -> ""

  in Pictures
      [ translate (-300) 320
          (Scale 0.15 0.15
            (Text ("Revealed: " ++ show safeRevealed)))
      , translate (-200) 360
          (Scale 0.2 0.2 (Text statusText))
      ]


handleEvent :: Event -> GameState -> IO GameState

-- Difficulty selection
handleEvent (EventKey (Char '1') Up _ _) (GameState _ Menu) =
  newGame 8 8 10

handleEvent (EventKey (Char '2') Up _ _) (GameState _ Menu) =
  newGame 12 12 20

handleEvent (EventKey (Char '3') Up _ _) (GameState _ Menu) =
  newGame 16 16 40

-- custom mode
handleEvent (EventKey (Char '4') Up _ _) (GameState _ Menu) = do
  putStrLn "Enter number of rows:"
  rInput <- getLine
  putStrLn "Enter number of columns:"
  cInput <- getLine
  putStrLn "Enter number of mines:"
  mInput <- getLine

  case (readMaybe rInput, readMaybe cInput, readMaybe mInput) of
    (Just r, Just c, Just m)
      | r > 0 && c > 0 && m > 0 && m < r * c ->
          newGame r c m
    _ -> do
      putStrLn "Invalid input. Returning to menu."
      return (GameState [] Menu)

-- restart
handleEvent (EventKey (Char 'r') Up _ _) gs =
  case status gs of
    Playing -> newGame (rows gs) (cols gs) (countMines gs)
    Won     -> return (GameState [] Menu)
    Lost    -> return (GameState [] Menu)
    Menu    -> return gs

-- mouse clicks
handleEvent (EventKey (MouseButton LeftButton) Up _ mousePos) gs
  | status gs == Playing =
      return $ revealCell (toBoardPos mousePos gs) gs

handleEvent (EventKey (MouseButton RightButton) Up _ mousePos) gs
  | status gs == Playing =
      return $ flagCell (toBoardPos mousePos gs) gs

handleEvent _ gs = return gs


update :: Float -> GameState -> IO GameState
update _ gs = return gs

newGame :: Int -> Int -> Int -> IO GameState
newGame r c m = do
  b <- initBoard r c m
  return (GameState b Playing)

rows :: GameState -> Int
rows = length . board

cols :: GameState -> Int
cols gs = length (head (board gs))

countMines :: GameState -> Int
countMines gs =
  length [ () | row <- board gs, cell <- row, hasMine cell ]

toBoardPos :: (Float, Float) -> GameState -> Pos
toBoardPos (mx, my) gs =
  let r = rows gs
      c = cols gs
      col = floor ((mx + fromIntegral c * cellSize / 2) / cellSize)
      row = r - 1 - floor ((my + fromIntegral r * cellSize / 2) / cellSize)
  in (row, col)
