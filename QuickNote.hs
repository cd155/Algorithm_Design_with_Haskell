{-
    Quick note for writing Haskell
-}


-- add module
module QuickNote where

-- create a synonym 
type Nat = Int

-- declaration
mergeSort :: Ord a => [a] -> [a]

-- patten match
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort firstHalf) (mergeSort secondHalf)

    -- where cause
    where (firstHalf, secondHalf) = splitAt (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs

-- pattern match for list
merge (x:xs) (y:ys)

    -- condition statement
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- initialize a list
smArrX = [1, 4, 10, 100, 1000]

-- ADT (abstract data type) for binary tree
data BiTree a = Null | Node a (BiTree a) (BiTree a) deriving Show
