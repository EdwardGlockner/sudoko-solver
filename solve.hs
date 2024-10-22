module Main where  -- Define the Main module

import System.IO (readFile)
import Data.Maybe (isNothing)
import qualified Data.List as List  -- Import List for utility functions
import qualified Sudoku as S  -- Assuming Sudoku module contains Board type
import qualified SudokuSolver as Solver  -- Assuming SudokuSolver module contains the solving logic

-- Type alias for better readability
type Board = [[Int]]

-- Function to read a single board from a file
readBoard :: FilePath -> IO (Maybe Board)
readBoard filePath = do
    contents <- readFile filePath
    let linesOfBoard = lines contents
    if null linesOfBoard
       then return Nothing  -- Return Nothing if the file is empty
       else do
           let size = read (head linesOfBoard) :: Int  -- First line indicates the size
           let board = map (map read . splitByComma) (tail linesOfBoard)  -- Convert to Board
           return (Just board)  -- Always return the board

-- A simple function to split a string by commas
splitByComma :: String -> [String]
splitByComma [] = []
splitByComma xs = case break (== ',') xs of
    (before, []) -> [before]  -- No more commas, return what we have
    (before, _:after) -> before : splitByComma after  -- Recursively process the rest

-- Main function to read and solve the Sudoku puzzle
main :: IO ()
main = do
    --let filePath = "dataset/test.txt"  -- Change this to your desired board file
    let filePath = "dataset/board3.txt"
    maybeBoard <- readBoard filePath  -- Read the board
    case maybeBoard of
        Just board -> do
            solutionCount <- Solver.numSolutions board  -- Call the solver function
            putStrLn $ "Number of solutions: " ++ solutionsToString solutionCount
        Nothing -> putStrLn "Invalid board format."

-- Helper function to convert Solutions to String
solutionsToString :: Solver.Solutions -> String
solutionsToString Solver.NoSolution      = "No Solution"
solutionsToString Solver.UniqueSolution   = "Unique Solution"
solutionsToString Solver.MultipleSolutions = "Multiple Solutions"

