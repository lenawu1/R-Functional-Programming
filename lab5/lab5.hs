import Control.Monad

-- A.1
hr_solution :: [((Integer, Integer), (Integer, Integer), Integer)]
hr_solution = do
	i <- [1 ..]
	j <- [1 .. i - 1]
	k <- [1 .. j - 1]
	l <- [1 .. k - 1]
	guard $ (i^3 + l^3) == (j^3 + k^3)
	return (((i, l), (j, k), i^3 + l^3))
	
-- A.2
a2way1 :: Integer
a2way1 = sum (do 
	x <- [1 .. 999]
	guard $ x `mod` 3 == 0 || x `mod` 5 == 0
	return (x))
	

a2way2 :: Integer
a2way2 = sum (do
	x <- [1 .. 999]
	if x `mod` 3 == 0 || x `mod` 5 == 0 then return () else mzero
	return (x))
	

-- A.3
isPalindrome :: Integer -> Bool
isPalindrome a = (show a) == reverse (show a)

largestPalindrome :: Integer
largestPalindrome = maximum (do
	x <- [100 .. 999]
	y <- [x .. 999]
	guard $ (isPalindrome (x * y))
	return ((x * y)))

{-- The answer is 906609 --}

-- A.4 
type Expr = [Item]

data Item = N Int | O Op
	deriving Show
	
data Op = Add | Sub | Cat
	deriving Show

ops :: [Item] 
ops = [O Add, O Sub, O Cat]

exprs :: [Expr]
exprs = do
	o1 <- ops
	o2 <- ops
	o3 <- ops
	o4 <- ops
	o5 <- ops
	o6 <- ops
	o7 <- ops
	o8 <- ops
	return ((N 1) : o1 : (N 2) : o2 : (N 3) : o3 : (N 4) : o4 : 
		(N 5) : o5 : (N 6) : o6 : (N 7) : o7: (N 8) : o8 : [N 9])

-- normalize
normalize :: Expr -> Expr
normalize [N a] = [N a]
normalize ((N a):(O Cat):(N b):xs) = normalize ((N ((a * 10) + b)):xs)
normalize ((a):(O Sub):xs) = (a):(O Sub) : normalize xs
normalize ((a):(O Add):xs) = (a):(O Add) : normalize xs
normalize _ = error "this is not a valid expression"

-- evaluate
evaluate :: Expr -> Int
evaluate [N a] = a
evaluate (N a:(O Sub):N b:xs) =  evaluate ((N (a - b)):xs)
evaluate (N a:(O Add):N b:xs) =  evaluate ((N (a + b)):xs)
evaluate _ = error "this is not a valid expression"



-- Pick out the expressions that evaluate to a particular number.
find :: Int -> [Expr] -> [Expr]
find n = filter (\e -> evaluate (normalize e) == n)

-- Pretty-print an expression.
pprint :: Expr -> String
pprint [N i] = show i
pprint (N i : O Add : es) = show i ++ " + " ++ pprint es
pprint (N i : O Sub : es) = show i ++ " - " ++ pprint es
pprint (N i : O Cat : es) = show i ++ pprint es
pprint _ = error "pprint: invalid argument"

-- Run the computation and print out the answers.
run :: IO ()
run = mapM_ putStrLn $ map pprint $ find 100 exprs


-- B.1 
{-- 
desugar this: 
do  n1 <- [1..6]
	n2 <- [1..6]
	[]
	return (n1, n2)
	
Desugared:	
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [] >> return (n1, n2)
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [] >> [(n1, n2)]
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [] >>= \_ -> [(n1, n2)]
(by the definition of the >>= operator)
[1..6] >>= \n1 -> [1..6] >>= \n2 -> concatmap \_ -> [(n1, n2)] []

Now, we can see that we are calling concatmap with a function that will
be applied to the empty list. That means that the empty list will be
what is returned because concatmap maps on 0 elements. Thus we return
[].

--}

-- B.2
{-- 

first one:
[1..6] >>= \n1 -> [1..6] >>= \n2 -> return <anything> >> return (n1, n2)
[1..6] >>= \n1 -> [1..6] >>= \n2 -> return <anything> >> [(n1, n2)]
[1..6] >>= \n1 -> [1..6] >>= \n2 -> return <anything> >>= \_ -> [(n1, n2)]
[1..6] >>= \n1 -> [1..6] >>= \n2 -> concatmap \_ -> [(n1, n2)] return <anything>
[1..6] >>= \n1 -> [1..6] >>= \n2 -> concatmap \_ -> [(n1, n2)] [<anything>]

The concatmap function will run the function that takes any value (\_)
and then calls it on each element of the inputted list. Thus, each 
element of the <anything> list for each pair n1, n2 will be returned
as [(n1, n2)] because that is how the mapping operation works, thus 
we desugar it again and get:
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [(n1, n2)]

We now compare this to our desugaring of the second method:

[1..6] >>= \n1 -> [1..6] >>= \n2 -> return (n1, n2)
[1..6] >>= \n1 -> [1..6] >>= \n2 -> [(n1, n2)]

and we see we get the same result!
--}

-- B.3 

{-- 
let s = ["aaxybb", "aazwbb", "foobar", "aaccbb", "baz"] in
  do ['a', 'a', c1, c2, 'b', 'b'] <- s 
     return [c1, c2]
     
s >>= \y -> case y of 
	['a', 'a', c1, c2, 'b', 'b'] -> return (c1, c2)
	_ -> fail "Pattern match failure in do expression"
	
s >>= \y -> case y of 
	['a', 'a', c1, c2, 'b', 'b'] -> [(c1, c2)]
	_ -> fail "Pattern match failure in do expression"
	
This pattern matching then matches on all elements of s that start with
two a's, and ends with two b's, with two mystery characters in the
middle. this means that it will return the two middle characters in 
that case. [(c1, c2)] If that is not the case, the pattern match fails,  
and our fail throws the empty list, and thus does not add anything
to the final result. If we used the default fail in this case, 
it would give us an error and return an error message, but in the list
case our fail does not stop our calculation.

--}

-- B.4

{-- 
vs concat (map k m)


foldr ((++) . k) [] m 
foldr (\a b -> (++ . k) a b) [] [x1, x2 ..]
foldr (\a b -> ++ (k a) b) [] [x1, x2 ..]
foldr (\a b -> (k a) ++ b) [] [x1, x2 ..]
(k x1) ++ (k x2) ++ .. ++ []
[(k x1), (k x2), ... ]

concat (map k m) 
concat (map k [x1, x2 ..])
concat ([(k x1), (k x2) , ..]) 
(k x1) ++ (k x2) ++ .. ++ [] 
[(k x1), (k x2), ... ]

Thus, the two definitins are the same

if we have [] for m:
foldr ((++) . k) [] []
foldr (\a b -> (++ . k) a b) [] []
foldr (\a b -> ++ (k a) b) [] []
foldr (\a b -> (k a) ++ b) [] []
[]

concat (map k []) 
concat ([])
[]

Thus, with the empty list they are both the same as well.

--}

-- B.5
{--
The problem with this code is that the n type and the s type are not
of the same kind, which is why we get a type equality issue. Then,
we we use the (+) operator, we cannot add two different Num types 
together which gives us our error.
To solve this we make n and s into the same type. We can fix this
by casting both n and s into the same type before we use the (+)
operator, thus making them the same kind before adding and we will 
not get the type equality issue.
--}
