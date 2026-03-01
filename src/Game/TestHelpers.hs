module Game.TestHelpers where

import Game.Types

-- show board for testing
printBoard :: Board -> IO ()
printBoard b = mapM_ printRow b
  where
    printRow :: [Cell] -> IO ()
    printRow row = putStrLn $ concatMap showCell row

    showCell :: Cell -> String
    showCell c
      | flagged c        = "F "
      | not (revealed c) = ". "
      | hasMine c        = "* "
      | adjMines c == 0  = "  "
      | otherwise        = show (adjMines c) ++ " "

-- show board for testing fully revealed
printDebugBoard :: Board -> IO ()
printDebugBoard b = mapM_ printRow (zip [0..] b)
  where
    printRow :: (Int, [Cell]) -> IO ()
    printRow (rIdx, row) = putStrLn $ show rIdx ++ " " ++ concatMap showCell row

    showCell :: Cell -> String
    showCell c
      | flagged c        = "F "
      | hasMine c        = "* "
      | not (revealed c) = ". "
      | adjMines c == 0  = "  "
      | otherwise        = show (adjMines c) ++ " "

-- add coordinates
printDebugBoardWithCoords :: Board -> IO ()
printDebugBoardWithCoords b = do
    putStr "   "
    putStrLn $ concatMap (\i -> show i ++ " ") [0..(length (head b) - 1)]
    printDebugBoard b
