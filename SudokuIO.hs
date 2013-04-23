module SudokuIO where

import Control.Exception
import Data.Char
import SudokuExceptions
import SudokuSolver

-- Converts a Sudoku string into an [[Int]]
-- ** can add counter later for line length
format :: String -> [Int] -> [[Int]]
format [] l        = [reverse l]
format ('\n':' ':xs) l = (reverse l):(format xs [])
format ('\n':xs) l = (reverse l):(format xs []) 
format ('-':xs)  l = format xs (0:l)
format (x:xs) l 
	| isNumber x = format xs ((digitToInt x):l) 
	| otherwise = throw InvalidSudokuFormatException

-- Translates an [[Int]] back to a String for printing
deformat :: [[Int]] -> String
deformat []          = ""
deformat ([]:xs)     = '\n':(deformat xs)
deformat ((0:tl):xs) = '-':(deformat (tl:xs))
deformat ((h:tl):xs) = (intToDigit h):(deformat (tl:xs))

main :: IO ()
main = readFile "diabolicalSudoku.txt" >>= (\f -> putStrLn $ deformat $ solveGuess $ format (read f) [])
