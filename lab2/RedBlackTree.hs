module RedBlackTree where

-- A color is either Red or Black
data Color = Red | Black
    deriving Show
    
-- A red-black tree is either empty ("leaf") or a tree node with a color
-- two branches, and a value of type a
data Tree a = Leaf | Node Color (Tree a) a (Tree a)
    deriving Show

-- A.1
-- Return true if the given element is in the tree
member :: Ord a => a -> Tree a -> Bool
member _ Leaf = False
member a (Node _ l b r) | a < b = member a l
                        | a > b = member a r
                        | otherwise = True

-- A.2
-- Convert a Tree to a list
toList :: Tree a -> [a]
toList Leaf = []
toList (Node _ l b r) = (toList l) ++ [b] ++ (toList r)



-- A.3
-- Insert a new element into a Tree
insert :: Ord a => a -> Tree a -> Tree a
insert elem t = makeBlack (ins elem t) 
  where
    -- Insert an element into a tree.
    ins :: Ord a => a -> Tree a -> Tree a
    ins elem Leaf = Node Red Leaf elem Leaf  -- new nodes are colored red
    ins elem t@(Node color left elem' right) 
      | elem < elem' = balance color (ins elem left) elem' right
      | elem > elem' = balance color left elem' (ins elem right)
      | otherwise = t  -- element already in the tree; no insertion required

    -- Make the root of the tree black.
    makeBlack :: Tree a -> Tree a
    makeBlack Leaf = Leaf
    makeBlack (Node _ left elem right) = Node Black left elem right

    -- Balance a red-black tree under construction which may not satisfy
    -- invariants 2 and 3.
    balance :: Ord a => Color -> Tree a -> a -> Tree a -> Tree a
    balance Black (Node Red (Node Red l1 e1 r1) e2 r2) e t = 
        Node Red (Node Black l1 e1 r1) e2 (Node Black r2 e t)
    balance Black (Node Red l2 e2 (Node Red l1 e1 r1)) e t = 
        Node Red (Node Black l2 e2 l1) e1 (Node Black r1 e t)
    balance Black t e (Node Red (Node Red l1 e1 r1) e2 r2) =
        Node Red (Node Black t e l1) e1 (Node Black r1 e2 r2)
    balance Black t e (Node Red l2 e2 (Node Red l1 e1 r1)) = 
        Node Red (Node Black t e l2) e2 (Node Black l1 e1 r1)
    balance color l e r = Node color l e r  -- no balancing needed

-- A.4
-- Convert a list to a Tree
fromList :: Ord a => [a] -> Tree a
fromList l = foldr (insert) Leaf l

-- A.5
minDepth :: Tree a -> Int
minDepth Leaf = 0
minDepth (Node _ l _ r) = min ((minDepth l) + 1) ((minDepth r) + 1)

maxDepth :: Tree a -> Int
maxDepth Leaf = 0
maxDepth (Node _ l _ r) = max ((maxDepth l) + 1) ((maxDepth r) + 1)

-- A.6
testInvariant1 :: Ord a => Tree a -> Bool
testInvariant1 Leaf = True
testInvariant1 t@(Node _ l _ r) = 
    ((child_check t) && (testInvariant1 l) && (testInvariant1 r))
    where
        child_check :: Ord a => Tree a -> Bool
        child_check (Node _ (Node _ _ e1 _) e (Node _ _ e2 _)) = 
            (e1 < e) && (e2 > e)
        child_check (Node _ Leaf _ Leaf) = True
        child_check (Node _ Leaf e (Node _ _ e1 _)) = e < e1
        child_check (Node _ (Node _ _ e1 _) e Leaf) = e > e1
        child_check Leaf = True
        
-- A.7
is_black :: Color -> Bool
is_black Black = True
is_black _ = False

testInvariant2 :: Ord a => Tree a -> Bool
testInvariant2 Leaf = True
testInvariant2 (Node Black l _ r) = 
    (testInvariant2 l) && (testInvariant2 r)
testInvariant2 (Node Red l@(Node c1 _ _ _) _ r@(Node c2 _ _ _)) =
    (is_black c1) && (is_black c2) && (testInvariant2 l) && (testInvariant2 r)
testInvariant2 (Node Red Leaf _ r@(Node c1 _ _ _)) = 
    is_black c1 && testInvariant2 r
testInvariant2 (Node Red l@(Node c1 _ _ _) _ _) =
    (is_black c1) && (testInvariant2 l)
testInvariant2 (Node Red Leaf _ Leaf) = True     

-- A.8
testInvariant3 :: Tree a -> Bool
testInvariant3 t = allEqual (leafCounts t 0)
  where
    -- Given a tree, return a list of the count of black nodes on every path
    -- from the root of the tree to a leaf.
    leafCounts :: Tree a -> Int -> [Int]
    leafCounts Leaf n = [n]
    leafCounts (Node Black left _ right) n = 
        (leafCounts left (n + 1)) ++ (leafCounts right (n + 1))
    leafCounts (Node Red left _ right) n = 
        (leafCounts left n) ++ (leafCounts right n)

    -- Return True if all the elements of a list are equal.
    allEqual :: Ord a => [a] -> Bool
    allEqual [] = True
    allEqual [_] = True
    allEqual (x:r@(y:_)) | x == y = allEqual r
                         | otherwise = False 
                                        

    
type Set a = Tree a

empty :: Set a
empty = Leaf

toSet :: Ord a => [a] -> Set a
toSet = fromList


-- B.1
isSubset :: Ord a => Set a -> Set a -> Bool
isSubset s1 s2 = all (\x -> member x s2) (toList s1)



-- B.2
eqSet :: Ord a => Set a -> Set a -> Bool
eqSet s1 s2 = (isSubset s1 s2) && (isSubset s2 s1)

-- B.3
union :: Ord a => Set a -> Set a -> Set a
union s1 s2 = foldr (insert) s2 (toList s1)


-- B.4
intersection :: Ord a => Set a -> Set a -> Set a
intersection s1 s2 = 
    foldr (\x r -> if member x s1 then insert x r else r) empty (toList s2)

-- B.5 
difference :: Ord a => Set a -> Set a -> Set a
difference s1 s2 = 
    foldr (\x r -> if not (member x s2) then insert x r else r) empty (toList s1)
