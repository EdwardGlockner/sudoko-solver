module Main where

import System.IO
import System.Directory (listDirectory)
import Data.Char (isDigit)
import Control.Monad (forM_)
import SudokuSolver (numSolutions, Board, Solutions(..))

-- Read a single board from a file
readBoard :: FilePath -> IO Board
readBoard filePath = do
    contents <- readFile filePath
    let linesOfBoard = lines contents
        size = read (head linesOfBoard) :: Int  -- First line indicates the size
        board = map (map read . splitByComma) (tail linesOfBoard)  -- Convert to Board
    return board

-- A simple function to split a string by commas
splitByComma :: String -> [String]
splitByComma [] = []
splitByComma xs = case break (== ',') xs of
    (before, []) -> [before]  -- Change '' to [] for empty remainder
    (before, _:after) -> before : splitByComma after

-- Print the number of solutions to the terminal
printSolution :: FilePath -> Solutions -> IO ()
printSolution filePath solutions = do
    let result = case solutions of
                    NoSolution         -> "No solution"
                    UniqueSolution     -> "Unique solution"
                    MultipleSolutions   -> "Multiple solutions"
    putStrLn $ filePath ++ ": " ++ result  -- Print the result to the terminal

-- Solve all boards in the dataset folder
solveAllBoards :: IO ()
solveAllBoards = do
    --let inputDir = "testdatasets"
    let inputDir = "dataset"
    files <- listDirectory inputDir
    forM_ files $ \file -> do
        let inputPath = inputDir ++ "/" ++ file
        board <- readBoard inputPath
        let solutions = numSolutions board
        printSolution inputPath solutions  -- Print the number of solutions

-- Main entry point of the program
main :: IO ()
main = do
    solveAllBoards
    putStrLn "All boards processed!"

