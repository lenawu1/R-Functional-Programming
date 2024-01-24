 --
-- Sudoku.hs
--

--
-- This program reads a Sudoku problem from a file and
-- outputs the solution to stdout.
--

module Main where

import Control.Monad
import Data.Array.IO
import Data.Char
import Data.List
import System.Environment
import System.Exit
import System.IO

usage :: IO ()
usage = hPutStrLn stderr $ "usage: sudoku filename"

type Sudoku = IOArray (Int, Int) Int


-- Read a file's contents into a Sudoku array.
readSudoku :: FilePath -> IO Sudoku
readSudoku f = do
  s <- readFile f
  let ls = lines s in
    if okLines ls
       then newListArray ((1, 1), (9, 9)) (map charToInt (concat ls))
       else error "readSudoku: invalid string input"
  where
    -- Check that the string input is a valid representation of a Sudoku board.
    okLines :: [String] -> Bool
    okLines ss =
      and [length ss == 9,
           all (\s -> length s == 9) ss,
           all okChar (concat ss)]

    okChar :: Char -> Bool
    okChar '.' = True
    okChar c | ord c >= ord '0' && ord c <= ord '9' = True
    okChar _ = False

    charToInt :: Char -> Int
    charToInt '.' = 0
    charToInt c   = ord c - ord '0'


checkempty :: Sudoku -> (Int, Int) -> IO Bool
checkempty s (x, y) = do
        val <- readArray s (x, y)
        if (val == 0) then return (False)
            else return (True)
    
getNext :: (Int, Int) -> (Int, Int)
getNext (x, 9) = (x + 1, 1)
getNext (x, y) = (x, y + 1)
    
    -- Solve a Sudoku board starting from location (i, j).
    -- All "previous" locations are assumed to have been filled.
    -- If the board is solveable, return True; if not, return False.
    -- In the latter case the board will not have changed.
iter :: Sudoku -> (Int, Int) -> IO Bool
iter s (x, y) | x > 9 = return True
iter s (x, y) = do
	--if (x == 0) then do (return True)
		oklist <- getOKValues s (x, y)
		filled <- checkempty s (x, y)
		let nextval = getNext (x, y)

		if filled then (iter s nextval)
				else (iter' s (x, y) oklist)

                
        
    -- Try to solve the board using all possible currently-valid
    -- values at a particular location.
    -- If the board is unsolveable, reset the location to a zero
    -- (unmake the move) and return False.
iter' :: Sudoku -> (Int, Int) -> [Int] -> IO Bool
iter' _ _ [] = do return (False)
iter' s (x, y) (l:ls) = do
    let nextval = getNext (x, y)
    writeArray s (x, y) l
    result <- iter s nextval
    if result then (return (True)) 
        else do
			writeArray s (x, y) 0
			(iter' s (x, y) ls)
        

            
             

    -- Get a list of indices that could be in a particular location on the 
    -- board (no conflicts in row, column, or box).
getOKValues :: Sudoku -> (Int, Int) -> IO [Int]
getOKValues s (x, y) = do
    row <- (getRow s y) 
    col <- (getCol s x)
    box <- getBox s (x, y)
    return ( [1 .. 9] \\ ((union (union row col) box)) )

    --Return the ith row in a Sudoku board as a list of Ints.
getRow :: Sudoku -> Int -> IO [Int]
getRow s r = do
    mapM (\x -> (readArray s (x, r))) [1..9] 
        >>= return . (filter (/= 0))
        
        

    -- Return the ith column in a Sudoku board as a list of Ints.
getCol :: Sudoku -> Int -> IO [Int]
getCol s c = do
    mapM (\x -> (readArray s (c, x))) [1..9] >>= 
        return . (filter (/= 0))

-- this will take a corner of a box, and read the values in the box
readBox :: Sudoku -> (Int, Int) -> IO [Int]
readBox s (x, y) = do
	v1 <- readArray s (x, y)
	v2 <- (readArray s (x + 1, y))      
	v3 <- (readArray s (x + 2, y))
	v4 <- (readArray s (x, y + 1))
	v5 <- (readArray s (x, y + 2))
	v6 <- (readArray s (x + 1, y + 1))
	v7 <- (readArray s (x + 2, y + 1))
	v8 <- (readArray s (x + 1, y + 2))
	v9 <- (readArray s (x + 2, y + 2))
	return (filter (/= 0) (v1:v2:v3:v4:v5:v6:v7:v8:[v9]))
        

-- Return the box covering location (i, j) as a list of Ints.
getBox :: Sudoku -> (Int, Int) -> IO [Int]
getBox s (x, y) = do 
    let xspot = (((x - 1) `div` 3) * 3) + 1
    let yspot = (((y - 1) `div` 3) * 3) + 1
    readBox s (xspot, yspot)




-- Solve a Sudoku board.
-- Do this by iterating through the board, incrementing the unfilled numbers
-- by 1 until the right solution is found.
-- Return True if a solution is found, else false.
-- If a solution is found, the board contents will have mutated to the solution.
solveSudoku :: Sudoku -> IO Bool
solveSudoku s = (iter s (1, 1))


-- Print a Sudoku board to stdout.
printSudoku :: Sudoku -> IO ()
printSudoku s = iter s 1 1
  where
    iter :: Sudoku -> Int -> Int -> IO ()
    iter s i j = 
      unless (i > 9)
        (do c <- readArray s (i, j)
            putChar $ intToChar c
            if j == 9 
               then putChar '\n' >> iter s (i + 1) 1
               else iter s i (j + 1))

    intToChar :: Int -> Char
    intToChar 0 = '.'
    intToChar n | n >= 1 && n <= 9 = intToDigit n
    intToChar m = error $ "printSudoku: invalid integer in array: " ++ show m


main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
     then usage >> exitFailure
     else
       do sudoku <- readSudoku (head args) -- read board contents into array
          solved <- solveSudoku sudoku
          if solved
             then printSudoku sudoku >> exitSuccess
             else putStrLn "No solution exists." >> exitFailure

