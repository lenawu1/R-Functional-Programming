import Control.Monad
import Control.Monad.State
import Data.IORef



-- While loop in the IO monad.
whileIO :: IO Bool -> IO () -> IO () 
whileIO test block = 
  do b <- test 
     when b (block >> whileIO test block)

-- While loop in the state monad.
whileState :: (s -> Bool) -> State s () -> State s ()
whileState test body = 
  do s0 <- get
     when (test s0)
          (do modify (execState body)
              whileState test body)
  
-- A.1            
factIO :: Integer -> IO Integer
factIO x | x < 0 = error "Argument must be non-negative"
factIO x = do 
    c <- newIORef x
    total <- newIORef 1
    whileIO 
        (do 
            counter <- readIORef c 
            return (counter /= 0))
        (do 
            retval <- readIORef total
            counter <- readIORef c
            writeIORef total (retval * counter)
            writeIORef c (counter - 1))
    readIORef total
    
-- A.2
factState :: Integer -> Integer
factState n | n < 0 = error "Argument must be non-negative"
factState n = evalState factHelper (n, 1)
    where
        factHelper :: State (Integer, Integer) Integer
        factHelper = do 
            whileState 
                (\(x, _) -> x /= 0)
                (do 
                    (x, ret) <- get
                    put ((x - 1), (ret * x)))
            (_, ret) <- get
            return (ret)
                    
-- A.3 
fibIO :: Integer -> IO Integer
fibIO n | n < 0 = error "Argument must be non-negative"
fibIO n = do 
    count <- newIORef n
    f1  <- newIORef 1
    f2  <- newIORef 0
    whileIO 
        (do 
            counter <- readIORef count 
            return (counter /= 0))
        (do 
            f1' <- readIORef f1
            f2' <- readIORef f2
            counter <- readIORef count
            writeIORef f1 (f1' + f2')
            writeIORef f2 (f1')
            writeIORef count (counter - 1))
    readIORef f2
    
-- A.4
fibState :: Integer -> Integer
fibState n | n < 0 = error "Argument must be non-negative"
fibState n = evalState fibHelper (n, 1, 0)
    where
        fibHelper :: State (Integer, Integer, Integer) Integer
        fibHelper = do
            whileState (\(x, _, _) -> x /= 0)
                (do 
                    (x, f1, f2) <- get
                    put ((x - 1), (f1 + f2), f1))
            (_, _, returnval) <- get
            return (returnval)

                
-- B.1
data Reader r b = Reader (r -> b)

runReader :: Reader r a -> r -> a
runReader (Reader f) = f


{-

we now can define these functions using reader
f :: a -> Reader r b
g :: b -> Reader r c
h :: a -> Reader r c

Define our three functions non-monadically:
f' :: (a, r) -> b

g' :: (b, r) -> c

h' :: (a, r) -> c
h' (x, r) = 
	let y = (f' (x, r)) in
	let z = (g' (y, r)) in
	(z, r)

now we can write:
f'' :: a -> r -> b
f'' x r = f' (x, r)

g'' :: b -> r -> c
g'' x r = g' (x, r)

h'' :: a -> r -> c
h'' x r = h' (x, r)

We can desgugar to 
f'' :: a -> r -> b
f'' x = (\r -> f' (x, r))
DO IT SIMILARLY FOR THE OTHER TWO (parellel thingys)

now we can say:
f x = Reader (\r -> f (x, r))
g x = Reader (\r -> g (x, r))
h x = Reader (\r -> let y = (f x) r
					let z = (g y) r in
					(z r))

from monadic composition operator we can see:

h = f >=> g 

h x = f x >=> g x
h x = f x >>= g
Reader (\r -> let y = (f x) r in let z = (g y) r in (z r)) = f x >>= g

we can now substitute mx for f x
Reader (\r -> let y = mx r in let z = (g y) r in (z r)) = mx >>= g

Substitute in runReader
Reader (\r -> let y = (runReader mx r) in runReader (g y) r) = mx >>= g

Replace all the y with x, and g with f
mx >>= f = Reader (\r -> let x = (runReader mx r) in runReader (f y) r)


Now, we will get return:
We can write return' as

return' (x, r) = x

return'' x = (\r -> x)

Then we can now see: 
return x = Reader (\r -> x)		
					


-}
