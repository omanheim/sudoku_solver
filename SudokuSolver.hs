module SudokuSolver where

import SudokuExceptions
import Data.Char
import Data.List
import Control.Exception

----- PART 1: Simple Solver -----

-- Safely returns the tail of a list
sHead :: [a] -> a
sHead [] = throw HeadException 
sHead (x:_) = x

-- Safely returns the tail of a list
sTail :: [a] -> [a]
sTail [] = throw TailException 
sTail (_:xs) = xs

-- Converts a column-oriented matrix to a row-oriented matrix
colToRow :: [[Int]] -> [[Int]]
colToRow ([]:_) = []
colToRow rm     = (map sHead rm):(colToRow (map sTail rm))

-- Helper for rowToBox function below that recursively breaks down the
-- sudoku into box units
rTBHelper :: [[Int]] -> [[Int]]
rTBHelper [] = []
rTBHelper w  = (foldl (++) [] (take 3 w)):(rTBHelper $ drop 3 w)

-- Converts a row-oriented matrix to a box-oriented matrix
rowToBox :: [[Int]] -> [[Int]]
rowToBox ([]:_) = []
rowToBox rm     = (rTBHelper $ (map (take 3) rm)) ++ (rowToBox (map (drop 3) rm))

-- Return true if the input list contains exactly the numbers 1-9
verifyGroup :: [Int] -> Bool
verifyGroup l = and $ map (\x -> elem x l) [1..9]

-- Folds a function across each row of an [[Int]]
applyRows :: ([Int] -> a) -> (a -> b -> b) -> b -> [[Int]] -> b
applyRows ag f b = foldr f b . map ag

-- Folds a function across each column of an [[Int]]
applyCols :: ([Int] -> a) -> (a -> b -> b) -> b -> [[Int]] -> b
applyCols _ _ b ([]:_) = b
applyCols ag f b l = f (ag (map sHead l)) (applyCols ag f b (map sTail l))

-- Helper for applyBoxes function below
aBHelper :: [[Int]] -> [[Int]]
aBHelper [] = []
aBHelper w  = (foldl (++) [] (take 3 w)):(aBHelper $ drop 3 w)

-- Folds a function through each "box" unit of an [[Int]]
applyBoxes :: ([Int] -> a) -> (a -> b -> b) -> (b -> b -> b) -> b -> [[Int]] -> b
applyBoxes _ _ _ b ([]:_) = b
applyBoxes ag f c b l = let boxes = map (take 3) l in
	c (foldr f b $ map ag $ aBHelper boxes) (applyBoxes ag f c b (map (drop 3) l)) 

-- Helper function for boxToRow function below
bTRHelper :: [[Int]] -> [[Int]]
bTRHelper [] = []
bTRHelper ([]:[]:[]:xs) = bTRHelper xs
bTRHelper xs = (foldr (++) [] $ map (take 3) $ take 3 xs):(bTRHelper ((map (drop 3) (take 3 xs)) ++ (drop 3 xs)))

-- Converts a box-oriented matrix into a row-oriented matrix.
boxToRow :: [[Int]] -> [[Int]]
boxToRow xs = bTRHelper cc 
	where
	ls = [(take 3 xs),(take 3 $ drop 3 xs),(take 3 $ drop 6 xs)]
	tp = transpose ls
	cc = foldr (++) [] tp

-- Checks if a (row-oriented) Sudoku is validly solved
isSolved :: [[Int]] -> Bool
isSolved rm = verifyRows rm && verifyCols rm && verifyBoxes rm
	where 
	verifyRows  = applyRows verifyGroup (&&) True
	verifyCols  = applyCols verifyGroup (&&) True
	verifyBoxes = applyBoxes verifyGroup (&&) (&&) True

-- Checks if one value is missing from a line
oneMissing :: [Int] -> Int
oneMissing l | length (filter (==0) l) == 1 = sHead $ filter 
	(\x -> not $ elem x l) [1..9]
oneMissing l = -1

-- Replaces a zero in a line with a given value
replaceZero :: [Int] -> Int -> [Int]
replaceZero (0:xs) i = (i:xs)
replaceZero (x:xs) i   = x:(replaceZero xs i)

-- Completes a line if it is missing a single value
fillSinglesGroup :: [Int] -> [Int]
fillSinglesGroup l | not $ oneMissing l == -1 = replaceZero l (oneMissing l)
fillSinglesGroup l 			      = l 

-- Complete one step of a simple solver: if a single value is missing from a 
-- row, then a column, then a box, fill it in
fillSingles :: [[Int]] -> [[Int]]
fillSingles = fillSinglesBoxes . fillSinglesCols . fillSinglesRows
	where
	fillSinglesRows = applyRows fillSinglesGroup (:) []
	fillSinglesCols = transpose . applyCols fillSinglesGroup (:) []
	fillSinglesBoxes = boxToRow . applyBoxes fillSinglesGroup (:) (++) []

-- Apply fillSingles until either the puzzle is solved, in which case returning
-- (theSolution, True), or until no more progress is made, returning
-- (ourProgress, False)
solveSimple :: [[Int]] -> ([[Int]], Bool)
solveSimple xs
	| xs == a   = (a, False)
	| isSolved a = (a, True)
	| otherwise = solveSimple a
	where a = fillSingles xs

----- PART 2: AC3 Solver -----

-- Domain is a list of possible values a cell can take on
type Domain = [Int]

-- Board is a matrix of domains, one for each cell in the 9x9 puzzle
type Board = [[Domain]]

-- A location on a board
type Coord = (Int, Int)

-- A relation between two locations on the board; generally, the domain of the
-- first location is being constrained by the domain of the second
type Arc = (Coord, Coord)

-- Returns the value at the position in the matrix given by the coord argument
navigate :: [[a]] -> Coord -> a
navigate m (0,0) = sHead $ sHead m
navigate m (x,0) = navigate ((sTail $ sHead m):(sTail m)) (x-1,0)
navigate m (x,y) = navigate (sTail m) (x,y-1)

-- Remove a value from a list of domains, coupled with a boolean telling if
-- anything actually happened
deleteFromRow :: [Domain] -> Int -> Int -> ([Domain], Bool)
deleteFromRow [] _ _ = ([], False)
deleteFromRow (x:xs) y r
	| y == 0    = ((delete r x):xs, elem r x)
	| otherwise = let (d, b) = deleteFromRow xs (y-1) r in (x:d, b)

-- Recurses until we are on the right row and then deletes a value from
-- the domains in that row
deleteFromDomain :: [[Domain]] -> Coord -> Int -> ([[Domain]], Bool)
deleteFromDomain (h:tl) (x, y) r | y == 0 = 
	let (d, b) = deleteFromRow h x r in (d:tl, b)
deleteFromDomain (h:tl) (x, y) r          = 
	let (d, b) = deleteFromDomain tl (x, y-1) r in (h:d, b)

-- Helper for domainify; converts a list of ints to a list of domains;
-- 0 represents "-" so a cell with that value takes on [1..9], otherwise the
-- domain is the single value
domainifyRow :: [Int] -> [Domain]
domainifyRow [] = []
domainifyRow (x:xs)
	| x == 0    = [1..9]:(domainifyRow (xs))
	| otherwise = [x]:(domainifyRow (xs))

-- Converts a puzzle to a [[Domain]] using the domainifyRow helper
domainify :: [[Int]] -> [[Domain]]
domainify []      = []
domainify ([]:xs) = domainify xs
domainify (x:xs) = (domainifyRow x):(domainify xs)

-- Helper for undomainify; converts a list of domains into a list of ints;
-- if a domain only has one element, that is the value in the output, otherwise
-- the value is 0, corresponding to -
undomainifyRow :: [Domain] -> [Int]
undomainifyRow [] = []
undomainifyRow (x:xs) = case x of
	[a] -> a:(undomainifyRow xs)
	otherwise -> 0:(undomainifyRow xs) 

-- Converts a [[Domain]] into an actual puzzle representation
undomainify :: [[Domain]] -> [[Int]]
undomainify [] = []
undomainify ([]:xs) = undomainify xs
undomainify (x:xs) = (undomainifyRow x):(undomainify xs)

-- Return a list of all the arcs beginning at a certain coordinate; i.e. a list 
-- of all arcs of the form (x, y), where x is the input and y is a cell that
-- constrains the domain of x
addArcs :: Coord -> [Arc]
addArcs p@(x,y) = foldr f [] $ (cols ++ rows ++ boxes)
	where
	cols = delete p $ map (\l -> (x,l)) $ [0..8]
	rows = delete p $ map (\l -> (l,y)) $ [0..8]
	boxes = delete p boxesW
	boxesW = [(a,b) | a <- [xStart..(xStart + 2)], 
		b <- [yStart..(yStart + 2)]]
	xStart = (div x 3) * 3
	yStart = (div y 3) * 3
	f = (\o l -> let i = coord o in if not $ elem i l then (i:l) else l)
	coord j = (p,j)

-- Takes in a coordinate and a list of arcs and returns a list of arcs that
-- is the input list with all arcs of the form (x, y), where y is the input
-- coord and x is a coord it constrains, which were not in the input list,
-- added to the list's end
arcsTo :: Coord -> [Arc] -> [Arc]
arcsTo p@(x,y) al = foldl f al $ map (\l -> (l,p)) $ (cols ++ rows ++ boxes)
	where
	cols = delete p $ map (\l -> (x,l)) $ [0..8]
	rows = delete p $ map (\l -> (l,y)) $ [0..8]
	boxes = delete p boxesW
	boxesW = [(a,b) | a <- [xStart..(xStart + 2)], 
		b <- [yStart..(yStart + 2)]]
	xStart = (div x 3) * 3
	yStart = (div y 3) * 3
	f = (\l o -> if not $ elem o l then (l ++ [o]) else l) 

-- Gets the initial list of arcs
initQueue :: [Arc]
initQueue = foldr (\x y -> addArcs x ++ y) [] [(x,y) | x <- [0..8], y <- [0..8]]

-- Takes in a domain matrix and an arc and returns a Just [[Domain]] 
-- constrained by that arc or Nothing if constraining by that arc caused there
-- to be no solution
arcReduce :: [[Domain]] -> Arc -> Maybe [[Domain]]
arcReduce m (a,b) = 
	case (navigate m b) of
		[x] -> case (deleteFromDomain m a x) of
			(d, True) -> Just d
			(_, False) -> Nothing
		_ -> Nothing 

-- Calls arcReduce until the arc queue is empty, returning either 
-- Just [[Domain]] which is our progress toward a solution after one iteration
-- of AC3 or Nothing if there is no solution with this input
processArcs :: [[Domain]] -> [Arc] -> Maybe [[Domain]]
processArcs m [] = Just m
processArcs m (x@(a,b):xs) = case (arcReduce m x) of
	Just d -> case (navigate d a) of
		[] -> Nothing 
		_ -> processArcs d $ arcsTo a xs
	Nothing -> processArcs m xs

-- Takes in a board and applies solveSimple to it followed by the AC3 
-- algorithm.
solveHard :: [[Int]] -> Maybe [[Int]]
solveHard s = case solveSimple s of
	(c, True) -> Just c
	(c, False) -> case processArcs (domainify c) initQueue of
		Just d -> Just (undomainify d)
		Nothing -> Nothing

----- PART 3: Guessing -----

-- Finds the cell with the smallest unsolved domain
smallestDomain :: [[Domain]] -> Coord -> Coord -> Int -> Coord
smallestDomain [] _ c _ = c
smallestDomain ([]:xs) (x,y) c m = smallestDomain xs (0,y+1) c m
smallestDomain ((h:tl):xs) (x,y) c m
	| v < m && v > 1 = smallestDomain (tl:xs) (x+1,y) (x,y) v
	| otherwise    = smallestDomain (tl:xs) (x+1,y) c m
	where v = length h

-- Helper for guessCell; goes through a row until at the right cell, then fills
-- it in with the guessed value
guessCellRow :: [Domain] -> Int -> Int -> [Domain]
guessCellRow (_:tl) 0 i = [i]:tl
guessCellRow (h:tl) x i = h:(guessCellRow tl (x-1) i)

-- Takes in a coordinate and a value to insert and inserts it appropriately
guessCell :: [[Domain]] -> Coord -> Int -> [[Domain]]
guessCell (h:tl) (x,0) i = (guessCellRow h x i):tl
guessCell (h:tl) (x,y) i = h:(guessCell tl (x,y-1) i)

-- Takes in a board, finds the cell with the smallest unsolved domain, and
-- returns a list of boards with each possible value for that cell filled in
branchBoards :: Board -> [Board]
branchBoards d = map (\l -> guessCell d sd l) $ navigate d sd
	where sd = smallestDomain d (0,0) (0,0) 10

-- Takes in a list of boards, applies AC3 to the first one, checks if that 
-- solved it, returns if yes, otherwise branches and tries again
guessBoards :: [Board] -> Board
guessBoards (x:xs) = case solveHard $ undomainify x of
	Just u -> if isSolved u
 		  then domainify u
		  else guessBoards (xs ++ branchBoards x)
	Nothing -> guessBoards xs
guessBoards _ = throw UnsolvableSudokuException

-- Takes in a puzzle and solves it using the guessing algorithm
solveGuess :: [[Int]] -> [[Int]]
solveGuess d = undomainify $ guessBoards [(domainify d)]
