module Main where

import Game.Board
import qualified Game.GUI as GUI
import Game.Types
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
  -- Board parameters
  let rows = 10
      cols = 10
      mines = 10

  -- Initialize board
  b <- initBoard rows cols mines
  let initialState = GameState b Playing

  -- Window and background
  let window :: Display
      window = InWindow "Minesweeper" (600, 600) (100, 100)

      backgroundColor :: Color
      backgroundColor = greyN 0.7

      fps = 30

  -- Run Gloss GUI
  playIO
    window
    backgroundColor
    fps
    initialState
    GUI.draw -- GameState -> IO Picture
    GUI.handleMouse -- Event -> GameState -> IO GameState
    GUI.update -- Float -> GameState -> IO GameState
