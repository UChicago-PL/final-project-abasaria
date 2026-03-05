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

usableWidth :: Float
usableWidth = 700

usableHeight :: Float
usableHeight = 700


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
        (Scale 0.12 0.12 (Text "Custom game input in terminal"))
    ]


drawBoard :: GameState -> Picture
drawBoard gs =
  let r = rows gs
      c = cols gs
      cellSize = min (usableWidth / fromIntegral c)
                     (usableHeight / fromIntegral r)

  in Pictures
      [ translate x y (Pictures [drawCell cell cellSize, border cellSize])
      | (rowIdx, row) <- zip [0..] (board gs)
      , (colIdx, cell) <- zip [0..] row
      , let x = fromIntegral colIdx * cellSize - fromIntegral c * cellSize / 2 + cellSize/2
      , let y = fromIntegral (r - rowIdx - 1) * cellSize - fromIntegral r * cellSize / 2 + cellSize/2
      ]

border :: Float -> Picture
border size =
  color black (rectangleWire size size)

drawCell :: Cell -> Float -> Picture
drawCell c size
  | flagged c = color red (rectangleSolid size size)
  | not (revealed c) = color (greyN 0.5) (rectangleSolid size size)
  | hasMine c = color black (rectangleSolid size size)
  | adjMines c == 0 = color white (rectangleSolid size size)
  | otherwise =
      Pictures
        [ color white (rectangleSolid size size)
        , color black
            (translate (-size/4) (-size/4)
              (Scale (size/200) (size/200)
                (Text (show (adjMines c)))))
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

      statusPic =
        case status gs of
          Won  -> boldText green "YOU WON!  (Press R for Menu)"
          Lost -> boldText red   "GAME OVER  (Press R for Menu)"
          _    -> Blank

  in Pictures
      [ translate (-350) 360
          (Scale 0.15 0.15
            (Text ("Revealed: " ++ show safeRevealed)))
      , translate (-300) 0 statusPic
      ]


boldText :: Color -> String -> Picture
boldText col str =
  let base = Scale 0.3 0.3 (Text str)
      offsets =
        [ translate dx dy base
        | dx <- [-1.5, 0, 1.5]
        , dy <- [-1.5, 0, 1.5]
        ]
  in color col (Pictures offsets)


handleEvent :: Event -> GameState -> IO GameState

-- Difficulty selection
handleEvent (EventKey (Char '1') Up _ _) (GameState _ Menu) =
  newGame 8 8 10

handleEvent (EventKey (Char '2') Up _ _) (GameState _ Menu) =
  newGame 12 12 20

handleEvent (EventKey (Char '3') Up _ _) (GameState _ Menu) =
  newGame 16 16 40

-- Custom mode
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

-- Restart
handleEvent (EventKey (Char 'r') Up _ _) gs =
  case status gs of
    Playing -> newGame (rows gs) (cols gs) (countMines gs)
    Won     -> return (GameState [] Menu)
    Lost    -> return (GameState [] Menu)
    Menu    -> return gs

-- Mouse clicks
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
      cellSize = min (usableWidth / fromIntegral c)
                     (usableHeight / fromIntegral r)
      col = floor ((mx + fromIntegral c * cellSize / 2) / cellSize)
      row = r - 1 - floor ((my + fromIntegral r * cellSize / 2) / cellSize)
  in (row, col)
