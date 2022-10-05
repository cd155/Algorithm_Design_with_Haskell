module Sorting where
{-
    Passing sequentially over a list, 
    comparing each value to the one immediately after it.   
    If the first value is greater than the second, their positions are switched.
-}
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
    repeat steps for its first and second
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

{-
    Select a pivot
    move everything smaller than the pivot to the left
    move everything great than  the pivot to the right
    position pivot(s) in the middle
    repeat steps for its left and right
-}
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort xs = quickSort left ++ mid ++ quickSort right
    where p = xs !! (length xs `div` 2)
          (left,mid,right) = qsHelper p xs ([],[],[])

-- move elements to left, mid, and right segments
qsHelper :: Ord a => a -> [a] -> ([a],[a],[a])-> ([a],[a],[a])
qsHelper _ [] tri = tri
qsHelper p (x:xs) (left,mid,right)
    | x < p = qsHelper p xs (left++[x], mid, right)
    | x == p = qsHelper p xs (left, mid++[x], right)
    | otherwise = qsHelper p xs (left, mid, right++[x])

{-

-}
radixSort :: Ord a => [a] -> [a]
radixSort [] = []
