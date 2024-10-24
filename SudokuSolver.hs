-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module SudokuSolver(Board, Solutions(..), author, nickname, numSolutions) where
import Sudoku(Board, Solutions(..))
-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- 
This is an implementation of a Sudoko solver using Haskell, that aims to find if a given Sudoko
puzzle has no solution, one unique solution or several solutions. This solution utilizes a grid-
based representation of the Sudoko board, where each cell/slot/number in the board can be fixed
or contain potential number candidates. A fixed cell is a cell where the number is guaranteed to
be "correct", either through given hints or by the algorithm removing all other possibilities.

The algorithm begins by validating the current state of the board to ensure there are no 
conflicts. Next, it iteratively narrows down the possible candidates for each cell based on the 
existing values within the board. The algorithm then recursively attempts to fill in the cells 
marked as potential candidates, and using backtracking when it encounters a dead end. This 
approach allows the algorithm to explore all the possibilities of the board.

The core of the algorithm is in the given key functions:

1. `isGridInvalid`: This function checks whether the current Sudoku board contains any 
    invalid states, such as duplicate values in any row, column, or sub-grid. It uses 
    helper functions like `isValidRow` to evaluate if each row is valid or not.

2. `filterGridOptions`: This function removes impossible candidates for cells labled as Potential.
    As a results this narrows down the potential solutions. 

3. `solveSudoko`: This is a recursive function that uses a backtracking algorithm in order to 
    explore possible placements for each cell marked as Potenteial. It uses the function
    'filterGridOptions' and validates each potential state, eventually resulting in either a
    solved grid or that no solution exists.

4. `numSolutions`: This function runs the main algorithm and counts the number of solutions for a 
    given board. If the number of solutions reaches two it stops there and returns multiple 
    solutions.

This is a backtracking algorithm and inspiration for the implementation were taken online, some of 
the sources are:

    https://en.wikipedia.org/wiki/Backtracking
    https://opensourc.es/blog/constraint-solver-pruning/
    https://www.geeksforgeeks.org/sudoku-backtracking-7/
-}


import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)
import Data.Function (on)
import Control.Monad (guard)
import Control.Applicative ((<|>))

author :: String
author = "Edward Glockner"  

nickname :: String
nickname = "EpicSudokoSolver3000"

{- 
  CellStatus represents the state of a single slot/number/cell in the Sudoku grid. 
  It can either be a fixed integer value that is assigned as a hint (or solved through the 
  algorithm) or a list of possible values that the cell can take.

  INVARIANT: For Fixed n, n is in [1..size of the Sudoko board]. For Potential xs, 
    xs contains unique integers in [1..size of the Sudoko board].
-}
data CellStatus = Fixed Int | Potential [Int] deriving (Show, Eq)
type Row = [CellStatus]
type SudokoGrid = [Row]

{- 
  boardTypeToCellType board
  Converts the board representation into a grid of CellStatus.
  The previous board datatype is a list och lists of integers, and is converted
  to a grid which is a list of rows which containts the CellStatus of each cell.
  
  PRE: The board is a valid Sudoku board.
  RETURNS: A grid (a list of rows) where each cell is either Fixed (has a
    decided value) or Potential (a list of possible values).
  
  EXAMPLES:
  > boardTypeToCellType [[5,3,0],[0,7,0],[0,0,0]]
  [[Fixed 5, Fixed 3, Potential [1,2,4,6,8,9]],[Potential [1,2,4,6,8,9], Fixed 7, 
    Potential [1,2,4,6,8,9]],[Potential [1,2,3,4,6,7,8,9], Potential [1,2,3,4,6,8,9], 
    Potential [1,2,3,4,6,7,8,9]]]
-}
boardTypeToCellType :: Board -> [Row]
boardTypeToCellType board = let size = length board in map (map (`convertToCell` size)) board

{- 
  convertToCell n size
  Helper function to boardTypeToCellType that converts a number n into a CellStatus 
  based on its value.

  RETURNS: Fixed n if n > 0; otherwise, Potential values from 1 to size.
  
  EXAMPLES:
  > convertToCell 0 9
  Potential [1,2,3,4,5,6,7,8,9]

  > convertToCell 5 9
  Fixed 5
-}
convertToCell :: Int -> Int -> CellStatus
convertToCell 0 size = Potential (generatePossibleValues size)
convertToCell n _   = Fixed n

{- 
  generatePossibleValues size
  Helper functino to convertToCell that generates a list of possible values from 1 to the given 
  size of the Sudoko board.

  RETURNS: A list of integers representing possible values.
  
  EXAMPLES:
  > generatePossibleValues 9
  [1,2,3,4,5,6,7,8,9]
  > generatePossibleValues 4
  [1,2,3,4]
-}
generatePossibleValues :: Int -> [Int]
generatePossibleValues size = [1..size]

{- 
  printBoard board
  Prints the Sudoku board in a readable format.

  SIDE EFFECTS: Outputs the board to the console.
  
  EXAMPLES:
  > printBoard [[Fixed 5, Fixed 3, Potential [1,2,4,6,8,9]],[Potential [1,2,4,6,8,9], Fixed 7, 
        Potential [1,2,4,6,8,9]],[Potential [1,2,3,4,6,7,8,9], Potential [1,2,3,4,6,8,9], 
        Potential [1,2,3,4,6,7,8,9]]]

  Outputs:
  5 3 [1,2,4,6,8,9]
  [1,2,4,6,8,9] 7 [1,2,4,6,8,9]
  [1,2,3,4,6,7,8,9] [1,2,3,4,6,8,9] [1,2,3,4,6,7,8,9]
-}
printBoard :: [[CellStatus]] -> IO ()
printBoard board = putStrLn $ unlines formattedRows
  where
    formattedRows = map (unwords . map printCell) board

{- 
  printCell cellStatus
  Helper function to printBoard that converts a CellStatus to its string representation.
  RETURNS: A string representation of the cell.
  
  EXAMPLES:
  > printCell (Fixed 5)
  "5"
  > printCell (Potential [1,2,3])
  "[1,2,3]"
-}
printCell :: CellStatus -> String
printCell (Potential xs) = "[" ++ unwords (map show xs) ++ "]"
printCell (Fixed num) = show num

{- 
  filterGridOptions grid
  Applies filterCellOptions across rows, columns, and subgrids of the grid.

  RETURNS: Just updated grid or Nothing if any cell cannot be updated.
  
  EXAMPLES:
  > filterGridOptions [[Fixed 5, Fixed 3, Potential [1,2,4]],[Potential [1,2,3], Fixed 7, Potential [1,2,4]]]
  Just [[Fixed 5, Fixed 3, Potential [1,4]],[Potential [1,2,3], Fixed 7, Potential [1,2,4]]]
-}
filterGridOptions :: SudokoGrid -> Maybe SudokoGrid
filterGridOptions = stabilize filterStep -- apply filterStep until stable
  where
    filterStep grid = do
      rowsUpdated <- traverse filterCellOptions grid
      colsUpdated <- fmap List.transpose . traverse filterCellOptions . List.transpose $ rowsUpdated
      subGridsUpdated <- fmap subGridsToRows . traverse filterCellOptions . subGridsToRows $ colsUpdated
      return subGridsUpdated

{- 
  filterCellOptions cells
  Helper functio to filterGridOptions that filters cell options based on fixed values in the row.

  RETURNS: Just a list of updated CellStatus or Nothing if any cell cannot be updated.
  
  EXAMPLES:
  > filterCellOptions [Fixed 5, Potential [1,2,3,4], Potential [2,3,5]]
  Just [Fixed 5, Potential [1,2,3,4], Potential [2,3]]
  > filterCellOptions [Fixed 5, Potential [], Fixed 3]
  Nothing
-}
filterCellOptions :: [CellStatus] -> Maybe [CellStatus]
filterCellOptions cells = do
    let fixedValues = [x | Fixed x <- cells]  
    let updatedCells = map (updateCellOptions fixedValues) cells 
    if any isNothing updatedCells then Nothing else Just (map fromJust updatedCells)

{- 
  updateCellOptions fixedValues cellStatus
  Helper function to filterCellOptions that updates the options in a Potential CellStatus based 
  on fixed values.

  updateCellOptions is applied to individual cells while filterCellOptions is applied to 
  multiple cells in a row or column.

  RETURNS: Updated CellStatus wrapped in Maybe.
  
  EXAMPLES:
  > updateCellOptions [5] (Potential [1,2,3,4,5])
  Just (Potential [1,2,3,4])
  > updateCellOptions [3] (Fixed 3)
  Just (Fixed 3)
-}
updateCellOptions :: [Int] -> CellStatus -> Maybe CellStatus
updateCellOptions fs (Potential xs) = createCell (xs List.\\ fs)  
updateCellOptions _ (Fixed x) = Just (Fixed x)

{- 
  createCell values
  Creates a CellStatus based on a list of values.
  RETURNS: Just Fixed if there's one value, Just Potential if multiple values, or Nothing if empty.
  
  EXAMPLES:
  > createCell [5]
  Just (Fixed 5)
  > createCell [1, 2, 3]
  Just (Potential [1,2,3])
  > createCell []
  Nothing
-}
createCell :: [Int] -> Maybe CellStatus
createCell []  = Nothing
createCell [y] = Just $ Fixed y
createCell ys  = Just $ Potential ys

{- 
  stabilize f x
  Applies a function f to x until the result no longer changes.
  The purpose of this function is to iteratively fiter the possible values for each cell in the 
  Sudoko grid until no further changes occur. The filtering is applied until the grid reaches
  a state where no more cells can be updated.

  RETURNS: The stabilized value.
  
  EXAMPLES:
  > stabilize (\x -> if x < 5 then return (x + 1) else return x) 0
  5
-}
stabilize :: (Monad m, Eq a) => (a -> m a) -> a -> m a
stabilize f x = do
    x' <- f x
    if x' == x then return x else stabilize f x'

{- 
  subGridsToRows grid
  Converts a grid into subgrids for processing.

  RETURNS: A list of rows representing subgrids.
  
  EXAMPLES:
  > subGridsToRows [[Fixed 5, Fixed 3],[Potential [1,2,4], Fixed 7]]
  [[Fixed 5, Fixed 3], [Potential [1,2,4], Fixed 7]]
-}
subGridsToRows :: SudokoGrid -> SudokoGrid
subGridsToRows grid =
  let size = length grid
      subgridSize = round (sqrt (fromIntegral size))
  in concatMap (map concat . List.transpose) $ -- conctanate and transpose into rows
    splitRowIntoSubRow subgridSize [splitRowIntoSubRow subgridSize row | row <- grid]

{- 
  splitRowIntoSubRow n xs
  Helper function to subGridsToRows that splits a list into sub-lists of size n.
  This function is used in order to split each row into sub-rows that corresponds to the sub-grid
  of the Sudoko. 
  RETURNS: A list of lists, each containing at most n elements.
  
  EXAMPLES:
  > splitRowIntoSubRow 3 [1,2,3,4,5,6,7]
  [[1,2,3],[4,5,6],[7]]
-}
splitRowIntoSubRow :: Int -> [a] -> [[a]]
splitRowIntoSubRow _ [] = []
splitRowIntoSubRow n xs = take n xs : splitRowIntoSubRow n (drop n xs)

{- 
  potentialGrids grid
  Generates the next potential grids to explore based on the current grid.

  RETURNS: A tuple of two grids for recursive backtracking.
  
  EXAMPLES:
  > potentialGrids [[Fixed 5, Potential [1,2,4]], [Potential [1,2,3], Fixed 7]]
  ([[Fixed 5, Fixed 1], [Potential [1,2,4], Fixed 7]], [[Fixed 5, Fixed 2], [Potential [1,2,3], Fixed 7]])
-}
potentialGrids :: SudokoGrid -> (SudokoGrid, SudokoGrid)
potentialGrids grid =
  let size = length grid
      indexedCells = zip [0..] (concat grid)
      openCells = filter (isCellOpen . snd) indexedCells
      -- Find the cell with the minimum possibilities, making it fixed
      (i, first@(Fixed _), rest) = fixCell . List.minimumBy (compare `on` possibilityCount) $ openCells
  -- Return two new grids: one with the first possibility fixed and the other with the rest of the possibilities.
  in (replace2D size i first grid, replace2D size i rest grid)

{- 
  isCellOpen cellStatus
  Checks if a cell is open, which means it has multiple possible values 
  that it can still take.

  RETURNS: True if the cell is Potential (i.e., it has multiple
  potential values), otherwise False (the cell is fixed).

  EXAMPLES:
  > isCellOpen (Fixed 5)
  False
  > isCellOpen (Potential [1,2,3])
  True
-}
isCellOpen :: CellStatus -> Bool
isCellOpen (Potential _) = True
isCellOpen _            = False

{- 
  possibilityCount (index, Cell)
  Helper function to potentialGrids that Counts the number of possibilities for a given cell.
  The input is a tuple where the first element is the index of the cell in the grid, and the
  second element is the cell status.

  RETURNS: The number of possible values for the cell.
  
  EXAMPLES:
  > possibilityCount (0, Potential [1,2,3])
  3
  > possibilityCount (1, Fixed 5)
  1
-}
possibilityCount :: (Int, CellStatus) -> Int
possibilityCount (_, Potential xs) = length xs
possibilityCount (_, Fixed _) = 1

{- 
  fixCell (index, Cell)
  Helper function to potentialGrids that fixes the first possible value of the cell and 
  returns it along with any remaining possibilities. The input is a tuple where the first element
  is the index of the cell in the grid and the second element is the cell status.

  RETURNS: A tuple containing the index of the cell, the Fixed value, and the remaining Potential 
  values.

  EXAMPLES:
  > fixCell (0, Potential [1, 2])
  (0, Fixed 1, Fixed 2)
  > fixCell (1, Potential [3, 4, 5])
  (1, Fixed 3, Potential [4, 5])
-}
fixCell :: (Int, CellStatus) -> (Int, CellStatus, CellStatus)
fixCell (i, Potential [x, y]) = (i, Fixed x, Fixed y)
fixCell (i, Potential (x:xs)) = (i, Fixed x, Potential xs)
fixCell _ = error "Unexpected case"

{- 
  replace2D size i value grid
  Helper function to potentialGrids that replaces a cell in a 2D grid at the given index i
  with the specified value.

  RETURNS: The updated Sudoku grid.

  EXAMPLES:
  > replace2D 2 1 (Fixed 8) [[Fixed 5, Fixed 3], [Fixed 6, Potential [1, 2]]]
  [[Fixed 5, Fixed 3], [Fixed 8, Potential [1, 2]]]
-}
replace2D :: Int -> Int -> CellStatus -> SudokoGrid -> SudokoGrid
replace2D size i v = let (x, y) = (i `quot` size, i `mod` size) in replace x (replace y (const v))
  where
    replace p f xs = [if i == p then f x else x | (x, i) <- zip xs [0..]]

{- 
  solveSudoko grid
  Solves the given Sudoku grid by exploring possible configurations of the grid 
  using backtracking. The function first filters the grid to ensure all values are 
  valid and identifies cells that can potentially be filled. 
  It then recursively attempts to fill the cells with possible values. 

  The backtracking algorithm allows for exploring different possibilities by fixing a value in a cell 
  and then recursively solving the resulting grid. If a dead end is reached, the function backtracks 
  and tries the next possible value.

  RETURNS: A tuple containing the solved grid (if there is no solved grid it returns Nothing)
  and the number of solutions found (0, 1 or 2).
  
  EXAMPLES:
  > solveSudoko [[Fixed 1, Fixed 2, Fixed 3],[Potential [1,2,3], Fixed 1, Fixed 3], [Fixed 3, Fixed 2, Fixed 1]]
  (Just [[Fixed 1, Fixed 2, Fixed 3],[Fixed 2, Fixed 3, Fixed 4], [Fixed 3, Fixed 2, Fixed 1]], 1)
-}
solveSudoko :: SudokoGrid -> (Maybe SudokoGrid, Int)
solveSudoko grid = case filterGridOptions grid of
    Nothing -> (Nothing, 0)
    Just g -> solveSudoko' g

{- 
  solveSudoko' g
  A recursive helper function to solveSudoko that attempts to solve a Sudoku puzzle represented 
  by the grid `g` using a backtracking algorithm.

  The function evaluates the current grid state and performs the following:
  1.If the grid is found to be invalid it returns `Nothing` along with a count of 0 solutions.
  2.If the grid is complete it returns the completed grid and a count of 1 solution.
  3.If the grid is neither invalid nor complete, it generates two potential grids 
    to explore further. It recursively attempts to solve the first potential grid.
    If at least two solutions are found from the first attempt, it returns the solution 
    from the first grid and the count of solutions. If less than two solutions are found, 
    it then attempts to solve the second potential grid and returns the accumulated 
    counts of solutions from both attempts.

  RETURNS: 
  A tuple (Maybe SudokoGrid, Int) where:
    - A `Maybe SudokoGrid` contains the solved grid if a solution is found, or `Nothing`
      if the grid is invalid or unsolvable.
    - An `Int` indicating the total number of solutions found (could be 0, 1, or 2).

  EXAMPLES:
  EXAMPLES:
  > solveSudoko' [[Fixed 1, Fixed 2, Fixed 3],[Potential [1,2,3], Fixed 1, Fixed 3], [Fixed 3, Fixed 2, Fixed 1]]
  (Just [[Fixed 1, Fixed 2, Fixed 3],[Fixed 2, Fixed 3, Fixed 4], [Fixed 3, Fixed 2, Fixed 1]], 1)

  > solveSudoko' [[Fixed 1, Fixed 3, Fixed 3],[Potential [1,2,3], Fixed 1, Fixed 3], [Fixed 3, Fixed 2, Fixed 1]]
  (Nothing, 0)
-}
solveSudoko' :: SudokoGrid -> (Maybe SudokoGrid, Int)
solveSudoko' g
    | containsInvalidStates g = (Nothing, 0) 
    | isGridComplete g = (Just g, 1)
    | otherwise = 
        let (grid1, grid2) = potentialGrids g -- get the next two potential grids
            (sol1, count1) = solveSudoko grid1  -- solve first grid
        in if count1 >= 2 -- multiple solutions
           then (sol1, count1)  
           else let (sol2, count2) = solveSudoko grid2  -- solve second grid
                in (sol2, count1 + count2) 

{- 
  containsInvalidStates grid
  Checks if the Sudoku grid contains invalid states (like duplicate values).

  RETURNS: True if invalid, otherwise False.
  
  EXAMPLES:
  > containsInvalidStates [[Fixed 5, Fixed 3, Fixed 5],[Fixed 6, Fixed 7, Fixed 2],[Fixed 4, Fixed 8, Fixed 9]]
  True
  > containsInvalidStates [[Fixed 1, Fixed 3, Fixed 2],[Fixed 2, Fixed 1, Fixed 3],[Fixed 3, Fixed 2, Fixed 1]]
  False
-}
containsInvalidStates :: SudokoGrid -> Bool
containsInvalidStates grid = any (not . isValidRow) grid ||
                     any (not . isValidRow) (List.transpose grid) ||
                     any (not . isValidRow) (subGridsToRows grid)

{- 
  isValidRow row
  Checks if a row contains valid (unique) numbers.
  RETURNS: True if valid, otherwise False.
  
  EXAMPLES:
  > isValidRow [Fixed 3, Fixed 1, Fixed 3]
  False
  > isValidRow [Fixed 3, Fixed 1, Fixed 2]
  True
-}
isValidRow :: Row -> Bool
isValidRow row = let values = [x | Fixed x <- row]
                 in length values == length (List.nub values)

{- 
  isGridComplete grid
  Checks if the Sudoku grid is complete (no Potential values left, only Fixed).

  RETURNS: True if complete, otherwise False.
  
  EXAMPLES:
  > isGridComplete [[Fixed 1, Fixed 3, Fixed 2],[Fixed 2, Fixed 1, Fixed 3],[Fixed 3, Fixed 2, Fixed 1]]
  True
  > isGridComplete [[Potential [1,2,3] 1, Fixed 3, Fixed 2],[Fixed 2, Fixed 1, Fixed 3],[Fixed 3, Fixed 2, Fixed 1]]
  False
-}
isGridComplete :: SudokoGrid -> Bool
isGridComplete = null . filter isCellOpen . concat

{- 
  numSolutions grid
  Returns the number of solutions for the given Sudoku grid.

  RETURNS: The number of solutions found.
  
  EXAMPLES:
  > solveSudoko [[Fixed 1, Fixed 2, Fixed 3],[Potential [1,2,3], Fixed 1, Fixed 3], [Fixed 3, Fixed 2, Fixed 1]]
  1
  > numSolutions [[Fixed 1, Fixed 3, Fixed 3],[Potential [1,2,3], Fixed 1, Fixed 3], [Fixed 3, Fixed 2, Fixed 1]]
  0
-}
numSolutions :: Board -> Solutions
numSolutions board = 
    let boardCells = boardTypeToCellType board -- convert to cell representation
        maybeUpdatedBoard = filterGridOptions boardCells
    in case maybeUpdatedBoard of
        Nothing -> NoSolution
        Just updatedBoard -> 
            let (solvedGrid, count) = solveSudoko updatedBoard -- solve the updated board
            in case count of
                0 -> NoSolution
                1 -> UniqueSolution
                _ -> MultipleSolutions

