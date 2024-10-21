module SudokuSolver(Board, Solutions(..), author, nickname, numSolutions) where

import Sudoku(Board, Solutions(..))
import qualified Data.List as List
import Data.Maybe (fromJust)
import Data.Function (on)
import Control.Monad (guard)
import Control.Applicative ((<|>))

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

author :: String
author = "Your Name"  -- Replace with your first and last name

nickname :: String
nickname = "Your Nickname"  -- Replace with your nickname for your solver

-- Define Cell type
data Cell = Fixed Int | Possible [Int] deriving (Show, Eq)

type Row = [Cell]
type Grid = [Row]

-- Function to create a list of possible values based on the size of the board
possibleValues :: Int -> [Int]
possibleValues size = [1..size]

-- Function to convert a board to a list of Rows of Cells
convertBoardToCells :: Board -> [Row]
convertBoardToCells board = let size = length board in map (map (\value -> convertToCell value size)) board

-- Convert an integer to a Cell, where 0 represents an empty cell.
convertToCell :: Int -> Int -> Cell
convertToCell 0 size = Possible (possibleValues size)
convertToCell n _ = Fixed n

-- Function to regroup cells back into rows
groupCells :: [Cell] -> Int -> [[Cell]]
groupCells cells size = chunksOf size cells

-- Custom chunksOf function to split a list into chunks of a given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Function to print the board
printBoard :: [[Cell]] -> IO ()
printBoard board = mapM_ putStrLn formattedRows
  where
    formattedRows = map (unwords . map showCell) board

-- Function to format each cell for display
showCell :: Cell -> String
showCell (Possible xs) = "[" ++ unwords (map show xs) ++ "]"
showCell (Fixed num) = show num

-- Function to prune cells in a single row, column, or subgrid
pruneCells :: [Cell] -> Maybe [Cell]
pruneCells cells = traverse pruneCell cells
  where
    fixeds = [x | Fixed x <- cells]

    pruneCell (Possible xs) = case xs List.\\ fixeds of
      []  -> Nothing
      [y] -> Just $ Fixed y
      ys  -> Just $ Possible ys
    pruneCell x = Just x

-- Convert the subgrids of a grid into rows for pruning
subGridsToRows :: Grid -> Grid
subGridsToRows grid =
  let size = length grid
      subgridSize = round (sqrt (fromIntegral size))  -- Dynamic subgrid size
      regrouped = concat [concatMap (take subgridSize) $ chunksOf subgridSize rows | rows <- chunksOf subgridSize grid]
  in regrouped

-- Prune the grid iteratively until no more changes occur
pruneGrid :: Grid -> Maybe Grid
pruneGrid = fixM pruneStep
  where
    pruneStep :: Grid -> Maybe Grid
    pruneStep grid = do
      rowsPruned <- traverse pruneCells grid
      colsPruned <- fmap List.transpose . traverse pruneCells . List.transpose $ rowsPruned
      subGridsPruned <- fmap subGridsToRows . traverse pruneCells . subGridsToRows $ colsPruned
      return subGridsPruned

    fixM :: Eq a => (a -> Maybe a) -> a -> Maybe a
    fixM f x = f x >>= \x' -> if x' == x then return x else fixM f x'

-- Check if the grid is fully filled (no possible cells)
isGridFilled :: Grid -> Bool
isGridFilled grid = null [() | Possible _ <- concat grid]

-- Check if the grid is invalid (duplicates or cells with no possibilities)
isGridInvalid :: Grid -> Bool
isGridInvalid grid =
  any isInvalidRow grid
  || any isInvalidRow (List.transpose grid)
  || any isInvalidRow (subGridsToRows grid)
  where
    isInvalidRow row =
      let fixeds = [x | Fixed x <- row]
          emptyPossibles = [x | Possible x <- row, null x]
      in hasDups fixeds || not (null emptyPossibles)

    hasDups :: [Int] -> Bool
    hasDups l = hasDups' l []

    hasDups' [] _ = False
    hasDups' (y:ys) xs
      | y `elem` xs = True
      | otherwise   = hasDups' ys (y:xs)

-- Generate the next two possible grids for backtracking
nextGrids :: Grid -> (Grid, Grid)
nextGrids grid =
  let size = length grid -- Get the size of the grid
      (i, first@(Fixed _), rest) = fixCell . List.minimumBy (compare `on` possibilityCount) . filter isPossible . zip [0..] . concat $ grid
  in (replace2D size i first grid, replace2D size i rest grid)
  where
    isPossible (_, Possible _) = True
    isPossible _ = False

    possibilityCount (_, Possible xs) = length xs
    possibilityCount (_, Fixed _) = 1

    fixCell (i, Possible [x, y]) = (i, Fixed x, Fixed y)
    fixCell (i, Possible (x:xs)) = (i, Fixed x, Possible xs)
    fixCell _ = error "Unexpected case"

    replace2D :: Int -> Int -> Cell -> Grid -> Grid
    replace2D size i v = let (x, y) = (i `quot` size, i `mod` size) in replace x (replace y (const v))
    replace p f xs = [if i == p then f x else x | (x, i) <- zip xs [0..]]

-- Solve the grid using backtracking and count the number of solutions
solve :: Grid -> IO (Maybe Grid, Int)
solve grid = case pruneGrid grid of
    Nothing -> return (Nothing, 0)  -- Pruning resulted in an impossible state
    Just g -> solve' g
  where
    solve' g
      | isGridInvalid g = return (Nothing, 0)
      | isGridFilled g  = return (Just g, 1)
      | otherwise = do
          let (grid1, grid2) = nextGrids g
          (sol1, count1) <- solve grid1
          if count1 >= 2 then return (sol1, count1) else do
              (sol2, count2) <- solve grid2
              return (sol2, count1 + count2)

-- Main function to solve the Sudoku and print intermediate steps
numSolutions :: Board -> IO Solutions
numSolutions board = do
    let boardCells = convertBoardToCells board
    putStrLn "Initial board:"
    printBoard boardCells
    
    let maybePrunedBoard = pruneGrid boardCells
    case maybePrunedBoard of
        Nothing -> do
            putStrLn "Pruning resulted in an impossible state."
            return NoSolution
        Just prunedBoard -> do
            putStrLn "\nBoard after pruning:"
            printBoard prunedBoard
            (solvedGrid, count) <- solve prunedBoard
            case count of
                0 -> do
                    putStrLn "\nNo solution found."
                    return NoSolution
                1 -> do
                    putStrLn "\nSolved Grid:"
                    printBoard (fromJust solvedGrid)
                    return UniqueSolution
                _ -> do
                    putStrLn "\nMultiple solutions found."
                    return MultipleSolutions  -- Return the correct Solutions type

