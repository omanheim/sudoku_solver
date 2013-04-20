module SudokuSolver where

import SudokuExceptions
import Data.Char
import Control.Exception

-- Safely returns the tail of a list
sHead :: [a] -> a
sHead [] = throw InvalidSudokuFormatException
sHead (x:_) = x

-- Safely returns the tail of a list
sTail :: [a] -> [a]
sTail [] = throw InvalidSudokuFormatException
sTail (_:xs) = xs

-- Converts a row-oriented matrix to a column-oriented matrix
rowToCol :: [[Int]] -> [[Int]]
rowToCol ([]:_) = []
rowToCol rm     = (map sHead rm):(rowToCol (map sTail rm))

rTBHelper :: [[Int]] -> [[Int]]
rTBHelper [] = []
rTBHelper w  = (foldl (++) [] (take 3 w)):(rTBHelper $ drop 3 w)

-- Converts a row-oriented matrix to a box-oriented matrix
rowToBox :: [[Int]] -> [[Int]]
rowToBox ([]:_) = []
rowToBox rm     = (rTBHelper $ (map (take 3) rm)) ++ (rowToBox (map (drop 3) rm))

-- Return true if the input list contains exactly the numbers 1-9
verifyGroup :: [Int] -> Bool
verifyGroup l = foldl (&&) True $ map (\x -> elem x l) [1..9]

-- Veritifes that all lists in the input list of lists are valid
verifyGroups :: [[Int]] -> Bool 
verifyGroups = foldl (&&) True . map verifyGroup

-- Checks if a (row-oriented) Sudoku is validly solved
isValid :: [[Int]] -> Bool
isValid rm = verifyGroups rm && (verifyGroups $ rowToCol rm) && (verifyGroups $ rowToBox rm)

-- Checks if one value is missing from a line
oneMissing :: [Int] -> Int
oneMissing l | length (filter (==0) l) == 1 = sHead $ filter (\x -> not $ elem x l) [1..9]
oneMissing l = -1

-- Replaces a zero in a line with a given value
replaceZero :: [Int] -> Int -> [Int]
replaceZero (0:xs) i = (i:xs)
replaceZero (x:xs) i   = x:(replaceZero xs i)

-- Completes a line if it is missing a single value
fillSinglesGroup :: [Int] -> [Int]
fillSinglesGroup l | not $ oneMissing l == -1 = replaceZero l (oneMissing l)
fillSinglesGroup l 			      = l 

-- Completes each line if it is missing a single value
fillSinglesGroups :: [[Int]] -> [[Int]]
fillSinglesGroups = map fillSinglesGroup

-- Solves for obvious numbers in a Sudoku
fillSingles :: [[Int]] -> [[Int]]
fillSingles rm = fillSinglesGroups $ rowToBox $ fillSinglesGroups $ rowToCol $ fillSinglesGroups $ rowToCol rm
