module Lab3ab where

-- A.1/ A.2
data Nat = Zero | Succ Nat deriving 
    (Eq, Show)

{--
natEq :: Nat -> Nat -> Bool
natEq Zero Zero = True
natEq (Succ x) (Succ y) = x == y
natEq x y = False

instance Eq Nat where
    (==) = natEq
    x /= y = not (x == y)

instance Show Nat where
    show Zero = "Zero"
    show (Succ x) = "Succ " ++ (show x)
--}

-- A.3
natOrd :: Nat -> Nat -> Bool
natOrd Zero Zero = True
natOrd (Succ _) Zero = False
natOrd Zero (Succ _) = True
natOrd (Succ x) (Succ y) = natOrd x y


instance Ord Nat where
    (<=) = natOrd 
    {--
    (>) x y = not (natOrd x y)
    (>=) x y = (natOrd y x)
    (<) x y = not (x >= y)
    --}


{-- derviving Ord from Haskell will  work on this case, because
Haskell will know how to recursively count the amount of Succs
becayse it can just compare the Nat inside of each of the sucs
in the natural number. --}


-- A.4
data SignedNat = Neg Nat | Pos Nat deriving (Show)

signEq :: SignedNat -> SignedNat -> Bool
signEq (Neg Zero) (Pos Zero) = True
signEq (Pos Zero) (Neg Zero) = True
signEq (Neg _) (Pos _) = False
signEq (Pos _) (Neg _) = False
signEq (Neg x) (Neg y) = x == y
signEq (Pos x) (Pos y) = x == y

signOrd :: SignedNat -> SignedNat -> Bool 
signOrd (Neg _) (Pos _) = True
signOrd (Pos Zero) (Neg Zero) = True
signOrd (Pos _) (Neg _) = False
signOrd (Neg x) (Neg y) = x >= y
signOrd (Pos x) (Pos y) = x <= y


instance Eq SignedNat where
    (==) = signEq
    
instance Ord SignedNat where
    (<=) = signOrd
    
{-- we cannot auto-define Eq because negative numbers will be the same
size as big positive numbers, but they are not actually equal but 
Haskell will evaluate them in the same way. We cannot do that for Ord, 
because the bigger negative numbers in our SignedNat, the computer will 
think they are postive because they are evaulated from left to right.--}

-- A.5
addNat :: Nat -> Nat -> Nat
addNat Zero y = y
addNat x Zero = x
addNat (Succ x) (Succ y) = (addNat x (Succ (Succ y)))

addSignedNat :: SignedNat -> SignedNat -> SignedNat
addSignedNat (Pos Zero) x = x
addSignedNat (Neg Zero) x = x
addSignedNat x (Pos Zero) = x
addSignedNat x (Neg Zero) = x
addSignedNat (Pos y) (Pos x) = Pos (addNat x y)
addSignedNat (Neg (Succ x)) (Pos (Succ y)) = addSignedNat (Neg x) (Pos y)
addSignedNat (Pos (Succ x)) (Neg (Succ y)) = addSignedNat (Pos x) (Neg y)
addSignedNat (Neg (Succ x)) (Neg (Succ y)) = Neg (addNat (Succ x) (Succ y))

negateSignedNat :: SignedNat -> SignedNat
negateSignedNat (Pos x) = Neg x
negateSignedNat (Neg x) = Pos x


multNat :: Nat -> Nat -> Nat
multNat Zero _ = Zero
multNat (Succ Zero) x = x
multNat (Succ x) y = addNat (multNat x y) y 


multSignedNat :: SignedNat -> SignedNat -> SignedNat
multSignedNat (Pos Zero) _ =  Pos Zero
multSignedNat (Neg Zero) _ =  Neg Zero
multSignedNat (Neg x) (Pos y) =  Neg (multNat x y)
multSignedNat (Neg x) (Neg y) =  Pos (multNat x y)
multSignedNat (Pos x) (Pos y) =  Pos (multNat x y)
multSignedNat (Pos x) (Neg y) =  Neg (multNat x y)


absSignedNat :: SignedNat -> SignedNat
absSignedNat (Neg x) = Pos x
absSignedNat y = y

sigSignedNat :: SignedNat -> SignedNat
sigSignedNat (Neg Zero) = Neg Zero
sigSignedNat (Pos Zero) = Pos Zero
sigSignedNat (Pos _) = Pos (Succ Zero)
sigSignedNat (Neg _) = Neg (Succ Zero)



fromIntHelp :: Integer -> Nat -> Nat
fromIntHelp 0 y = y
fromIntHelp x y | x < 0 = fromIntHelp (x + 1) (Succ y)
fromIntHelp x y = fromIntHelp (x - 1) (Succ y)

fromInt :: Integer -> SignedNat
fromInt x | x >= 0 = Pos (fromIntHelp x Zero)
fromInt x = Neg (fromIntHelp x Zero)


instance Num SignedNat where
    (+) = addSignedNat
    (*) = multSignedNat
    negate = negateSignedNat
    abs = absSignedNat
    signum = sigSignedNat
    fromInteger x = fromInt x
    


-- A.6
helperTo :: SignedNat -> Integer -> Integer
helperTo (Pos Zero) x = x
helperTo (Neg Zero) x = -x
helperTo (Pos (Succ y)) x = helperTo (Pos y) (x + 1)
helperTo (Neg (Succ y)) x = helperTo (Neg y) (x + 1)

signedNatToInteger :: SignedNat -> Integer
signedNatToInteger x = helperTo x 0

-- A.7
{-- 
Pos Zero and Neg Zero are the same number. To avoid this, I can just
make our SignedNat definition like so:

data UnaryInteger = Neg Nat | Pos Nat | Zero deriving (Show)
This way, whenever we are Zero, we will have the same value for each of
them and do not have to match on both positive and negative zero. 

--}
data UnaryInteger = Negative Nat | Positive Nat | Z deriving (Show)


-- A.8
factorial :: (Num a, Ord a) => a -> a
factorial x | x < 0 = error "cannot use negative numbers"
            | x == 0 = 1
            | otherwise = x * factorial (x - 1)


-- B.1
{-- 
1: >#<
Infix. This is because the result of this operation will be a string, 
and then if we compare the resulting string in any fixity to another
integer, we would get a type error.

2: +|
Infixl. This could also be infixr.
Ex) 3 +| 4 +| 7 = 7 +| 7 = 4 if we do infixl.
 3 +| 4 +| 7 = 3 +| 1 = 4 if we do infixr.
 
3. &<
Infixl
Ex) [1, 3, 4] &< 3 &< 7 &< 4 = [1, 3, 4, 3] &< 7 &< 4 = 
[1, 3, 4, 3, 7]  &< 4 = [1, 3, 4, 3, 7, 4]

4. >&&
Infixr 
Ex) 1 >&& 2 >&& [3] = 1 >&& [2, 2, 3] = [1, 1, 2, 2, 3]
--}

-- B.2
{-
+# 
This can be both Infixl and Infixr. (It can also be infix)
Its associativity should be Infix. This is because the results of this 
operation will change drastically depending on what order you compute
it. For example 3 +# 2 +# 800 can either be 3 or 1, depending on
if you do the left side first or the right side first. This means
that you should indicate which operations you do first depending on what
kind of result is desired. 
-}


