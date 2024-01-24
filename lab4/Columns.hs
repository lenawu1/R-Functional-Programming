module Main where
import System.Environment 
import System.Exit
import Data.Char
import System.IO

main :: IO()
main = do
    args <- getArgs
    -- get contents
    contents <- hGetContents stdin
    -- parse the args
    if (parseArgs args) || (length args < 2) then do
        putStr "usage: columns n1 n2 .. filename (each column n must be an integer that not 0 or negative) \n"
        exitFailure
        else return ()
    let c = getCols args
    
    -- reading from either the file, or the terminal
    if ((last args) == "-") then do 
        let l = lines contents
        everything l c
        else do
            filestr <- readFile (last args)
            let l = lines filestr
            everything l c
    
    
    
isNum :: String -> Bool
isNum [] = True
isNum (x:xs) = (isDigit x) && (isNum xs)

{-- This function will check that all of our arguments are valid --}
parseArgs :: [String] -> Bool
parseArgs [] = True
parseArgs (x:xs) = ((isNum x) && (parseArgs xs))


{-- gets the array of columns --}
getCols :: [String] -> [Int]
getCols [] = []
getCols (_:xs) | xs == [] = []
getCols (x:xs) = [read x] ++ (getCols xs)


{-- this function takes a string array (representing a line) and 
an array of ints representing the columns to return, and returns an
array that only contains those columns --}  
filterCols :: [String] -> [Int] -> [String]
filterCols _ [] = []
filterCols l (x:xs) | x <= (length l) = [l!!(x - 1)] ++ (filterCols l xs)
filterCols l (_:xs) = (filterCols l xs)


{-- does the final printing and parsing --}
everything :: [String] -> [Int] -> IO ()
everything [] _ = return ()
everything (x:xs) a = do
    let row = filterCols (words x) a
    putStr ((unwords row) ++ "\n")
    everything xs a
