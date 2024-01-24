module SparseMatrix where

import qualified Data.Map as M
import qualified Data.Set as S 

data SparseMatrix a =
  SM { bounds     :: (Integer, Integer),  -- number of rows, columns
       rowIndices :: S.Set Integer,       -- row indices with nonzeros
       colIndices :: S.Set Integer,       -- column indices with nonzeros
       vals       :: (M.Map (Integer, Integer) a) }  -- values
  deriving (Eq, Show)


-- C.1
getRows :: ((Integer, Integer), a) -> Integer
getRows ((x, _), _) = x

getCols :: ((Integer, Integer), a) -> Integer
getCols ((_, y), _) = y

check_indicies :: [((Integer, Integer), a)] -> (Integer, Integer) -> Bool
check_indicies [] _ = True
check_indicies (((x1, y1), _):xs) b = 
        x1 <= (fst b) && y1 <= snd(b) && (check_indicies xs b) && x1 > 0 && y1 > 0




sparseMatrix :: (Eq a, Num a) => 
  [((Integer, Integer), a)] -> (Integer, Integer) -> SparseMatrix a

sparseMatrix _ b | (fst b <= 1) || (snd b) <= 1 = error "array bounds must be greater than 1"
sparseMatrix x b | (check_indicies x b) == False = error "invalid bounds"
sparseMatrix x b = 
    let filtered = filter (\a -> (snd a) /= 0) x in
    let filtered2 = filter (\a -> (fst (fst a)) <= (fst b) && (snd (fst a)) <= (snd b)) filtered in
    SM b (S.fromList (foldr (\c r -> (getRows c) : r) [] filtered2)) (S.fromList (foldr (\c r -> (getCols c) : r) [] filtered2)) (M.fromList filtered)

-- C.2
addSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a

addSM (SM b1 _ _ _) (SM b2 _ _ _) | 
        b1 /= b2 = error "bounds are not equal"
addSM (SM b1 _ _ m1) (SM _ _ _ m2) = 
    let newmap = M.toList (M.unionWith (+) m1 m2) in
    sparseMatrix newmap b1
    
-- C.3
negateSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a
negateSM (SM b r c m) = SM b r c (M.map (negate) m)

-- C.4
subSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
subSM s1 s2 = addSM s1 (negateSM s2)

-- C.5
multVectors :: (Eq a, Num a) => (M.Map (Integer, Integer) a) -> (M.Map (Integer, Integer) a) -> Integer -> Integer -> a

multVectors m1 m2 x y = 
    let row = M.filterWithKey (\a _ -> (fst a) == x) m1 in
    let col = M.filterWithKey (\a _ -> (snd a) == y) m2 in
    let rowmap = M.mapKeys (snd) row in
    let colmap = M.mapKeys (fst) col in
    let values = M.intersectionWith (*) rowmap colmap in
    M.foldr (+) 0 values
    
coliter :: (Eq a, Num a) => (M.Map (Integer, Integer) a) -> 
    (M.Map (Integer, Integer) a) -> (M.Map (Integer, Integer) a) -> 
    [Integer] -> Integer -> (M.Map (Integer, Integer) a)
coliter _ _ h [] _ = h
coliter m1 m2 h (c:cs) r = coliter m1 m2 (M.insert (r, c) (multVectors m1 m2 r c) h) cs r


mulSM :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
mulSM (SM b1 _ _ _) (SM b2 _ _ _) |
  (snd b1) /= (fst b2)= error "invalid bounds!"
mulSM (SM b1 r1 _ m1) (SM b2 _ c2 m2) = 
    let rowlst = S.toList r1 in 
    let maplst = map coliter2 rowlst in 
    sparseMatrix (M.toList (foldr (M.union) M.empty maplst)) ((fst b1), (snd b2))
        where coliter2 row = coliter m1 m2 M.empty (S.toList c2) row
        
    
-- C.6
getSM :: Num a => SparseMatrix a -> (Integer, Integer) -> a
getSM (SM _ _ _ m1) i = M.findWithDefault 0 i m1

rowsSM :: (Eq a, Num a) => SparseMatrix a -> Integer
rowsSM (SM b1 _ _ _) = (fst b1)

colsSM :: (Eq a, Num a) => SparseMatrix a -> Integer
colsSM (SM b1 _ _ _) = (snd b1)

-- C.7
(<|+|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|+|>) = addSM

(<|-|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|-|>) = subSM

(<|*|>) :: (Eq a, Num a) => SparseMatrix a -> SparseMatrix a -> SparseMatrix a
(<|*|>) = mulSM

(<!>) :: Num a => SparseMatrix a -> (Integer, Integer) -> a
(<!>) = getSM

-- C.8
{-- 
It is difficult for us to come up with all the values needed for the Num
class in our matrix. For example, how can we come up with FromInteger in
our matrix. What matrix will the number 1 be? Since we can't define all 
of its traits, it makes no sense to have our matricies be a Num.
--}
