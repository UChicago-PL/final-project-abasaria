module Game.TestHelpers where

import Game.Types

-- | Display the board in ASCII (normal game view)
printBoard :: Board -> IO ()
printBoard b = mapM_ printRow b
  where
    printRow :: [Cell] -> IO ()
    printRow row = putStrLn $ concatMap showCell row

    showCell :: Cell -> String
    showCell c
      | flagged c        = "F "           -- Flagged cell
      | not (revealed c) = ". "           -- Hidden cell
      | hasMine c        = "* "           -- Revealed mine
      | adjMines c == 0  = "  "           -- Empty revealed
      | otherwise        = show (adjMines c) ++ " "

-- | Display the board in ASCII with all mines revealed (debug view)
printDebugBoard :: Board -> IO ()
printDebugBoard b = mapM_ printRow (zip [0..] b)
  where
    printRow :: (Int, [Cell]) -> IO ()
    printRow (rIdx, row) = putStrLn $ show rIdx ++ " " ++ concatMap showCell row

    showCell :: Cell -> String
    showCell c
      | flagged c        = "F "           -- Flagged cell
      | hasMine c        = "* "           -- Mine always shown
      | not (revealed c) = ". "           -- Hidden cell
      | adjMines c == 0  = "  "           -- Empty revealed
      | otherwise        = show (adjMines c) ++ " "

-- | Optional: Debug board with column numbers
printDebugBoardWithCoords :: Board -> IO ()
printDebugBoardWithCoords b = do
    putStr "   "
    putStrLn $ concatMap (\i -> show i ++ " ") [0..(length (head b) - 1)]
    printDebugBoard b
