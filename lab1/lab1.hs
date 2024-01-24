-- B.1.1
(+*) :: Double -> Double -> Double
(+*) x y = (x * x) + (y * y)
infixl 7 +*

-- B.1.2
(^||) :: Bool -> Bool -> Bool
(^||) False y = y
(^||) True x = not x
infixr 3 ^||

-- B.2
rangeProduct :: Integer -> Integer -> Integer
rangeProduct x y | y < x = error ("First argument greater than second")
rangeProduct x y | x == y = x
rangeProduct x y = x * (rangeProduct (x + 1) y)

-- B.3
prod :: [Integer] -> Integer
prod = foldr (*) 1


rangeProduct2 :: Integer -> Integer -> Integer
rangeProduct2 x y = prod [x .. y]


-- B.4.1
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (x:xs) (y:ys) = (f x y) : (map2 f xs ys)

-- B.4.2
map3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
map3 _ [] _ _ = []
map3 _ _ [] _ = []
map3 _ _ _ [] = []
map3 f (x:xs) (y:ys) (z:zs) = (f x y z) : (map3 f xs ys zs)

-- B.4.3
{-- 
dot = (sum .) . map2 (*)

dot lst1 lst2
(sum .) . map2 (*) lst1 lst2
(sum .) (map2 (*) lst1 lst2)
(\x -> sum . x) (map2 (*) lst1 lst2)
sum (map2 (*) lst1 lst2) 

--> this is what we want, thus they are the same

--}

-- B.5

w = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]
-- Sum is 233168


-- B.6
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve [y | y<-xs, y `mod` x /= 0] 

primes = sieve [x | x <- [2..]]
s = sum (takeWhile (<10000) primes)
-- Sum is 5736396

-- C.1 
sumList :: [Integer] -> Integer
sumList [] = 0
sumList (x:xs) = x + (sumList xs)

{-- The reason that the original code is bad is because it does not
use the built in pattern matching on the head and tail of a list, and
instead it uses head and tail definition. To change this we just change
the last line of pattern matching to x:xs and reference those when we
refer to the head or tail. --}

-- C.2
largest :: [Integer] -> Integer
largest [] = error "empty list"
largest (x:xs) | xs == [] = x
largest (x:xs) = max x (largest xs)

{-- the original code was bad for a couple reasons. First, instead of
checking the length of the list, we can just match on the empty list
for our original error check. Also, instead of matching on the list
in the second case, we can just match the tail with an empty list. 
Finally, in the recursive step we can use pattern matching on the 
head and tail of the list instead of using the build in head and tail
functions to determine the head and tail of the input. --}

-- D.1 
{-- 
fib 3
fib (3 - 1) + fib (3 - 2)
fib 2 + fib (3 - 2)
(fib 1 + fib 0) + fib (3 - 2)
(1 + fib 0) + fib (3 - 2)
(1 + 0) + fib (3 - 2)
1 + fib (3 - 2)
1 + fib 1
1 + 1
2
--}

-- D.2
{-- 
fact 3
3 * fact (3 - 1)
3 * fact (2)
3 * (2 * fact (2 - 1))
3 * (2 * fact 1)
3 * 2 * (1 * fact (1 - 1))
3 * 2 * (1 * fact 0)
3 * 2 * 1 * (0 * fact (0 - 1))
3 * 2 * 1 * (0 * fact -1)
This will continue forever.... :O

In order to fix this, we should reverse the order of the fact 0 and 
fact n so that fact 0 is matched when n = 0 instead of matching 0 with n,
like so:

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)
--}

-- D.3
{--
reverse [1, 2, 3]
iter [1, 2, 3] []
iter [2, 3] (1 : [])
iter [2, 3] [1]
iter [3] (2: [1])
iter [3] [2, 1]
iter [] (3 : [2, 1])
iter [] [3, 2, 1]
[3, 2, 1]

The time complexity of this function is O(n), where n is the length of
the input list. This is because this function takes the first member
of input list and places into the iter list, and it only does this once
for each number in the list. Thus, when the whole list is reversed we
only computed n steps.
--}

-- D.4
{--
reverse [1, 2, 3]
(reverse [2, 3]) ++ [1]
((reverse [3]) ++ [2]) ++ [1]
(((reverse []) ++ [3]) ++ [2]) ++ [1]
(([] ++ [3]) ++ [2]) ++ [1]
([3] ++ [2]) ++ [1]
(3: ([] ++ [2])) ++ [1]
(3 : [2]) ++ [1]
[3, 2] ++ [1]
3: ([2] ++ [1])
3: (2: ([] ++ 1))
3: (2: [1])
[3, 2, 1]

The reason why this is not linear time complexity, is because
each concatenation is done in O(n) time, and not O(1). Since we do not
do all the concatanations at the same time, we must do them on their own,
and the concat operator is dependent on how long the first list is. This
means that when we call our function, it will have a larger asmymtotic 
time complexity. Our total time complexity is O(n) to get to the
concat section, and then our concatting will take 1 + 2 + 3 + .... + n
steps for each individual concatenation. Thus, our total complexity will
be n + n(n + 1), which means it runs in O(n^2). 
--}

-- D.5


{--
head (isort [3, 1, 2, 5, 4])
head ( insert 3 isort [1, 2, 5, 4])
head (insert 3 (insert 1 isort [2, 5, 4]))
head (insert 3 (insert 1 (insert 2 isort [5, 4])))
head (insert 3 (insert 1 (insert 2 (insert 5 isort [4]))))
head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 isort [])))))
head (insert 3 (insert 1 (insert 2 (insert 5 (insert 4 [])))))
head (insert 3 (insert 1 (insert 2 (insert 5 [4]))))
head (insert 3 (insert 1 (insert 2 (4: insert 5 [])))
head (insert 3 (insert 1 (insert 2 (4: [5])))
head (insert 3 (insert 1 (insert 2 [4, 5]))
head (insert 3 (insert 1 ([2, 4, 5]))
head (insert 3 ([1, 2, 4, 5]))
head (1 : (insert 3 [2, 4, 5]))
1		(because it matches with (x: _)
--}

-- D.6.1
{-- 
foldr max 0 [1, 5, 3, -2, 4]
max 1 (foldr max 0 [5, 3, -2, 4])
max 1 (max 5 (foldr max 0 [3, -2, 4]))
max 1 (max 5 (max 3 (foldr max [-2, 4])))
max 1 (max 5 (max 3 (max -2 (foldr max 0 [4]))))
max 1 (max 5 (max 3 (max -2 (max 4 (foldr max 0 [])))))
max 1 (max 5 (max 3 (max -2 (max 4 (0))))
max 1 (max 5 (max 3 (4)))
max 1 (max 5 (4))
max 1 (5)
5
--}


-- D.6.2
{-- 
foldl max 0 [1, 5, 3, -2, 4]
foldl max (max 0 1) [5, 3, -2, 4]
foldl max (max (max 0 1) 5) [3, -2, 4]
foldl max (max (max (max 0 1) 5) 3) [-2, 4]
foldl max (max (max (max (max 0 1) 5) 3) -2) [4]
foldl max (max (max (max (max (max 0 1) 5) 3) -2) 4) []
(max (max (max (max (max 0 1) 5) 3) -2) 4)
(max (max (max (max 1 5) 3) -2) 4)
(max (max (max 5 3) -2) 4)
(max (max 5 -2) 4)
(max 5 4)
5

foldr requires less space than foldl because it does not hold the 
intermediate values because of the lazy evaluation.
--}


 





