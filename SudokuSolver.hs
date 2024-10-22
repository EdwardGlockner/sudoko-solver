module SudokuSolver(Board, Solutions(..), author, nickname, numSolutions) where

import Sudoku(Board, Solutions(..))
import qualified Data.List as List
import qualified Data.Map as Map
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

-- Check if a cell is possible
isPossible :: Cell -> Bool
isPossible (Possible _) = True
isPossible _            = False

-- Function to get exclusive possibilities from a row
exclusivePossibilities :: [Cell] -> [[Int]]
exclusivePossibilities row =
  let indexedCells = zip [1..] row
      possibleCells = filter (isPossible . snd) indexedCells
      valueMap = List.foldl' (\acc (i, Possible xs) -> List.foldl' (\acc' x -> Map.insertWith prepend x [i] acc') acc xs) Map.empty possibleCells
      filteredMap = Map.filter ((< 4) . length) valueMap
      resultMap = Map.foldlWithKey' (\acc x is -> Map.insertWith prepend is [x] acc) Map.empty filteredMap
      finalMap = Map.filterWithKey (\is xs -> length is == length xs) resultMap
  in Map.elems finalMap
  where
    prepend ~[y] ys = y:ys

-- Function to create a Cell from a list of possible values
makeCell :: [Int] -> Maybe Cell
makeCell ys = case ys of
  []  -> Nothing
  [y] -> Just $ Fixed y
  _   -> Just $ Possible ys

-- Prune cells by removing fixed values
pruneCellsByFixed :: [Cell] -> Maybe [Cell]
pruneCellsByFixed cells = traverse pruneCell cells
  where
    fixeds = [x | Fixed x <- cells]
    pruneCell (Possible xs) = makeCell (xs List.\\ fixeds)
    pruneCell x             = Just x

-- Prune cells using exclusive possibilities
pruneCellsByExclusives :: [Cell] -> Maybe [Cell]
pruneCellsByExclusives cells = case exclusives of
  [] -> Just cells
  _  -> traverse pruneCell cells
  where
    exclusives    = exclusivePossibilities cells
    allExclusives = concat exclusives
    pruneCell cell@(Fixed _) = Just cell
    pruneCell cell@(Possible xs)
      | intersection `elem` exclusives = makeCell intersection
      | otherwise                      = Just cell
      where
        intersection = xs `List.intersect` allExclusives

-- Fix point combinator for functions returning Maybe
fixM :: (Eq t, Monad m) => (t -> m t) -> t -> m t
fixM f x = f x >>= \x' -> if x' == x then return x else fixM f x'

-- Function to prune cells using both fixed values and exclusive possibilities
pruneCells :: [Cell] -> Maybe [Cell]
pruneCells cells = fixM pruneCellsByFixed cells >>= fixM pruneCellsByExclusives

-- Function to prune the grid iteratively until no more changes occur
pruneGrid :: Grid -> Maybe Grid
pruneGrid = fixM pruneStep
  where
    pruneStep :: Grid -> Maybe Grid
    pruneStep grid = do
      rowsPruned <- traverse pruneCells grid
      colsPruned <- fmap List.transpose . traverse pruneCells . List.transpose $ rowsPruned
      subGridsPruned <- fmap subGridsToRows . traverse pruneCells . subGridsToRows $ colsPruned
      return subGridsPruned

-- Convert the subgrids of a grid into rows for pruning
subGridsToRows :: Grid -> Grid
subGridsToRows grid =
  let size = length grid
      subgridSize = round (sqrt (fromIntegral size))  -- Dynamic subgrid size
  in concatMap (map concat . List.transpose) $ chunksOf subgridSize [chunksOf subgridSize row | row <- grid]

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
    isInvalidRow :: [Cell] -> Bool
    isInvalidRow row =
      let fixeds = [x | Fixed x <- row]
          emptyPossibles = [x | Possible x <- row, null x]
      in hasDups fixeds || not (null emptyPossibles)

    hasDups :: [Int] -> Bool
    hasDups l = hasDups' l []

    hasDups' :: [Int] -> [Int] -> Bool
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
    Nothing -> return (Nothing, 0)
    Just g -> solve' g

solve' :: Grid -> IO (Maybe Grid, Int)
solve' g
  | isGridInvalid g = return (Nothing, 0)
  | isGridFilled g  = do
      putStrLn "\nFound a solution:"
      printBoard g
      return (Just g, 1)
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
                    return MultipleSolutions

