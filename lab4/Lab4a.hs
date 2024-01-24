import Data.Char


-- A.1
myPutStrLn :: String -> IO ()
myPutStrLn "" = putChar '\n'
myPutStrLn (c:cs) = putChar c >> myPutStrLn cs

-- A.2
-- just delete the do, we dont need it!
greet :: String -> IO ()
greet name = putStrLn ("Hello, " ++ name ++ "!")

-- A.3
-- Ask the user for his/her name, then print a greeting.
greet2 :: IO ()
greet2 = do
  putStr "Enter your name: "
  name <- getLine
  putStr "Hello, "
  putStr name
  putStrLn "!"
    
-- way1
greetway1 :: IO ()
greetway1 = (putStr "Enter your name:") >> 
    (getLine >>= \name -> 
        putStr "Hello, " >> putStr name >> putStr "! \n")

-- way2
greetway2 :: IO ()
greetway2 = (putStr "Enter your name:") >> 
    getLine >>= \name -> case name of
        _ -> putStr ("Hello, " ++ name ++ "! \n")

-- A.4

-- Need to import this to get the definition of toUpper:

-- Ask the user for his/her name, then print a greeting.
-- Capitalize the first letter of the name.
greet3 :: IO ()
greet3 = do
  putStr "Enter your name: "
  (n:ns) <- getLine
  let name = toUpper n : ns
  putStr "Hello, "
  putStr name
  putStrLn "!"
 
 

greet3way1 :: IO ()
greet3way1 = 
    putStr "Enter your name: " >> getLine >>= \(n:ns) ->
        let name = toUpper n:ns in
        putStr "Hello, " >> putStr name >> putStrLn "!"

greet3way2 :: IO ()
greet3way2 = 
    putStr "Enter your name: " >> getLine >>=  \name -> 
        case name of 
        (n:ns) ->  
            let name' = toUpper n:ns in putStr "Hello, " >> 
            putStr name' >> putStrLn "!"
        _ -> fail "Pattern Match failure in do experession"


{-- The second way will go to the fail case if you don't enter anything
for your name because it won't match on n:ns. But instead, the first 
way will throw an error instead of reaching the fail case. --}
