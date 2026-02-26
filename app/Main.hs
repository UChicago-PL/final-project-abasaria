module Main where

import Graphics.Gloss.Interface.IO.Game
import Game.Types
import qualified Game.GUI as GUI

window :: Display
window = InWindow "Minesweeper" (800, 800) (100, 100)

backgroundColor :: Color
backgroundColor = greyN 0.8

fps :: Int
fps = 60

main :: IO ()
main = do
  let initialState = GameState [] Menu

  playIO
    window
    backgroundColor
    fps
    initialState
    GUI.draw
    GUI.handleEvent
    GUI.update
