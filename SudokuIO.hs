module SudokuIO where

import System.Environment
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
deformat :: [[Int]] -> Int -> Int -> String
deformat [] _ _          = ""
deformat ([]:xs) _ 2     = '\n':'\n':(deformat xs 0 0)
deformat ([]:xs) _ c     = '\n':(deformat xs 0 (c+1))
deformat ((0:tl):xs) 2 c = '-':' ':(deformat (tl:xs) 0 c)
deformat ((0:tl):xs) r c = '-':(deformat (tl:xs) (r+1) c)
deformat ((h:tl):xs) 2 c = (intToDigit h):' ':(deformat (tl:xs) 0 c)
deformat ((h:tl):xs) r c = (intToDigit h):(deformat (tl:xs) (r+1) c)

main :: IO ()
main = getArgs >>= \l ->
       readFile (head l) >>= \f ->
       putStrLn "Sudoku Solver!!!\n" >>= \_ ->
       putStrLn $ (\l -> deformat l 0 0) $ solveGuess $ format (read f) [] 
