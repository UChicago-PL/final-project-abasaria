module Game.DebugHelpers where

import Game.Types

-- show board for debugging
-- show mines and hidden cells, flagged cells marked
printDebugBoard :: Board -> IO ()
printDebugBoard b = mapM_ printRow (zip [0..] b)
  where
    printRow :: (Int, [Cell]) -> IO ()
    printRow (rIdx, row) = putStrLn $ show rIdx ++ " " ++ concatMap showCell row

    showCell :: Cell -> String
    showCell c
      | flagged c      = "F "
      | hasMine c      = "* "
      | not (revealed c) = ". "
      | adjMines c == 0 = "  "
      | otherwise       = show (adjMines c) ++ " "

printDebugBoardWithCoords :: Board -> IO ()
printDebugBoardWithCoords b = do
    putStr "   "
    putStrLn $ concatMap (\i -> show i ++ " ") [0..(length (head b) - 1)]
    printDebugBoard b
