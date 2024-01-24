module Main where
import System.Environment 
import System.Exit

main :: IO ()
main = do
	args <- getArgs
	if (length args /= 1) 
	then do 
		putStr "usage: reverse filename \n" 
		exitFailure 
		else return()
	filestr <- readFile (head args)
	let l = lines filestr
	let reversed = reverse l
	reversal reversed
	exitSuccess
    
reversal :: [String] -> IO ()
reversal [] = return ()
reversal (x:xs) = do 
	(putStr ((x) ++ "\n"))
	reversal xs
	
	
    
    
