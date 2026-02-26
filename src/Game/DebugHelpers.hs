module Game.DebugHelpers where

import Game.Types

-- | Display the board for debugging
-- Revealed mines and hidden cells are shown, flagged cells marked
printDebugBoard :: Board -> IO ()
printDebugBoard b = mapM_ printRow (zip [0..] b)
  where
    printRow :: (Int, [Cell]) -> IO ()
    printRow (rIdx, row) = putStrLn $ show rIdx ++ " " ++ concatMap showCell row

    showCell :: Cell -> String
    showCell c
      | flagged c      = "F "           -- Flagged cell
      | hasMine c      = "* "           -- Mine, always shown for debug
      | not (revealed c) = ". "         -- Hidden cell
      | adjMines c == 0 = "  "          -- Empty revealed
      | otherwise       = show (adjMines c) ++ " "

-- | Optional: Print column numbers at top for easier reference
printDebugBoardWithCoords :: Board -> IO ()
printDebugBoardWithCoords b = do
    putStr "   "
    putStrLn $ concatMap (\i -> show i ++ " ") [0..(length (head b) - 1)]
    printDebugBoard b
