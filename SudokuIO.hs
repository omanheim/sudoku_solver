module SudokuIO where

import Control.Exception
import Data.Char
import SudokuExceptions
import SudokuSolver

-- Converts a Sudoku string into an [[Int]]
-- ** can add counter later for line length
formatter :: String -> [Int] -> [[Int]]
formatter [] l        = [reverse l]
formatter ('\n':' ':xs) l = (reverse l):(formatter xs [])
formatter ('\n':xs) l = (reverse l):(formatter xs []) 
formatter ('-':xs)  l = formatter xs (0:l)
formatter (x:xs) l 
	| isNumber x = formatter xs ((digitToInt x):l) 
	| otherwise = throw InvalidSudokuFormatException

-- Translates an [[Int]] back to a String for printing
deformatter :: [[Int]] -> String
deformatter []          = ""
deformatter ([]:xs)     = '\n':(deformatter xs)
deformatter ((0:tl):xs) = '-':(deformatter (tl:xs))
deformatter ((h:tl):xs) = (intToDigit h):(deformatter (tl:xs))

main :: IO ()
main = readFile "validSudoku.txt" >>= (\f -> putStrLn $ show $ isValid $ formatter (read f) [])
