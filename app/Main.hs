module Main where

import Graphics.Gloss.Interface.IO.Game
import Game.Types
import Game.Board
import qualified Game.GUI as GUI

-- | Window settings
window :: Display
window = InWindow "Minesweeper" (600, 600) (100, 100)

backgroundColor :: Color
backgroundColor = greyN 0.7

fps :: Int
fps = 60

-- | Game parameters
rows, cols, mines :: Int
rows = 10
cols = 10
mines = 10

main :: IO ()
main = do
    -- 1. Generate initial board with mines
    b <- initBoard rows cols mines
    let initialState = GameState b Playing

    -- 2. Start Gloss interactive game
    playIO
      window
      backgroundColor
      fps
      initialState
      GUI.draw         -- GameState -> IO Picture
      GUI.handleMouse  -- Event -> GameState -> IO GameState
      GUI.update       -- Float -> GameState -> IO GameState
