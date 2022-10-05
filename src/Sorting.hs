module Sorting where

bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs
    | xs == bsInOnePass xs = xs
    | otherwise = bubbleSort (bsInOnePass xs)

-- bubble sort in one pass
bsInOnePass :: Ord a => [a] -> [a]
bsInOnePass [] = []
bsInOnePass [x] = [x]
bsInOnePass (x1:x2:xs) = min x1 x2: bsInOnePass (max x1 x2:xs)

{-
    divide the list into first half and second half
    sort the first half
    sort the second half
    merge first and second
-}
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort firstHalf) (mergeSort secondHalf)
    where (firstHalf, secondHalf) = splitAt (length xs `div` 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

pivotSort :: Ord a => [a] -> [a]
pivotSort [] = []
pivotSort [x] = [x]
pivotSort xs = pivotSort left ++ mid ++ pivotSort right
    where p = xs !! (length xs `div` 2)
          -- sorted list divide by three parts
          (left,mid,right) = pivotHelper p xs ([],[],[])

pivotHelper :: Ord a => a -> [a] -> ([a],[a],[a])-> ([a],[a],[a])
pivotHelper _ [] tri = tri
pivotHelper p (x:xs) (left,mid,right)
    | x < p = pivotHelper p xs (left++[x], mid, right)
    | x == p = pivotHelper p xs (left, mid++[x], right)
    | otherwise = pivotHelper p xs (left, mid, right++[x])


