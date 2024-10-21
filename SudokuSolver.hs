module SudokuSolver (Board, Solutions(..), author, nickname, numSolutions) where

import Sudoku (Board)

author :: String
author = "Your First and Last Name"  -- replace with your name

nickname :: String
nickname = "YourNickname"  -- replace with your nickname

-- Define a new Solutions data type
data Solutions = UniqueSolution | MultipleSolutions | NoSolution
    deriving (Show, Eq)

-- Returns the number of unique solutions or whether there are multiple or no solutions.
numSolutions :: Board -> Solutions
numSolutions board = countSolutions board
  where
    countSolutions b
      | isSolved b = UniqueSolution
      | otherwise =
          case findEmpty b of
            Nothing -> NoSolution
            Just (row, col) ->
              let validNumbers = possibleValues b (row, col)
              in if null validNumbers 
                 then NoSolution 
                 else classifySolutions validNumbers row col b

-- Checks if the board is solved (no empty cells)
isSolved :: Board -> Bool
isSolved b = all (all (/= 0)) b

-- Find the first empty cell using Minimum Remaining Values heuristic
findEmpty :: Board -> Maybe (Int, Int)
findEmpty b = foldr minNothing Nothing [(r, c) | r <- [0..size-1], c <- [0..size-1], b !! r !! c == 0]
  where
    size = length b
    minNothing :: (Int, Int) -> Maybe (Int, Int) -> Maybe (Int, Int)
    minNothing pos Nothing = Just pos
    minNothing pos (Just best)
      | length (possibleValues b pos) < length (possibleValues b best) = Just pos
      | otherwise = Just best

-- Get possible values for a given cell
possibleValues :: Board -> (Int, Int) -> [Int]
possibleValues b (r, c) = [num | num <- [1 .. size], valid b (r, c) num]
  where size = length b

-- Check if placing a number is valid
valid :: Board -> (Int, Int) -> Int -> Bool
valid b (r, c) num = notElem num (getRow r ++ getCol c ++ getBox r c)
  where
    size = length b
    getRow r = b !! r
    getCol c = [b !! i !! c | i <- [0..size-1]]
    getBox r c = [b !! i !! j | i <- [boxRowStart .. boxRowEnd], j <- [boxColStart .. boxColEnd]]
      where
        boxRowStart = (r `div` boxSize) * boxSize
        boxColStart = (c `div` boxSize) * boxSize
        boxRowEnd = boxRowStart + boxSize - 1
        boxColEnd = boxColStart + boxSize - 1
        boxSize = round (fromIntegral size ** 0.5)

-- Classify the number of solutions based on the count
classifySolutions :: [Int] -> Int -> Int -> Board -> Solutions
classifySolutions validNumbers row col b =
  let results = [numSolutions (placeNumber b (row, col) num) | num <- validNumbers]
  in case results of
       [] -> NoSolution
       _ -> if all (== UniqueSolution) results then UniqueSolution
            else if any (== MultipleSolutions) results then MultipleSolutions
            else NoSolution

-- Place a number on the board and return a new board
placeNumber :: Board -> (Int, Int) -> Int -> Board
placeNumber b (r, c) num = take r b ++ [take c (b !! r) ++ [num] ++ drop (c + 1) (b !! r)] ++ drop (r + 1) b

