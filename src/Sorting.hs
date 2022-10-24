module Sorting where

import Basics (Nat)
import Data.List (sort, sortOn, group)

{-
    Bubble Sort: 
    Passing sequentially over a list, 
    comparing each value to the one immediately after it.   
    If the first value is greater than the second, their positions are switched.
-}
bubbleSort :: Ord a => [a] -> [a]
bubbleSort xs
    | xs == sorted = xs
    | otherwise = bubbleSort sorted
    where sorted = bsInOnePass xs

-- bubble sort in one pass
bsInOnePass :: Ord a => [a] -> [a]
bsInOnePass [] = []
bsInOnePass [x] = [x]
bsInOnePass (x1:x2:xs) = min x1 x2: bsInOnePass (max x1 x2:xs)

{-
    Radix Sort: sort integer by its digits one by one
    max: the largest integer
-}
radixSort :: [Int] -> [Int]
radixSort [] = []
radixSort xs = radixHelper 1 (getIntegerSize max) xs
    where max = maximum xs

{-
    Sort the list base on the index of the Integer
    pos: the index of the Integer
    max: max size of the Integer
    xs: unsorted list
-}
radixHelper :: Nat -> Nat-> [Int] -> [Int]
radixHelper _ _ [] = []
radixHelper pos max xs
    | pos > max = xs
    | otherwise = radixHelper (pos+1) max (sortOn (getSingleDigit pos) xs)

-- Get the size of an Integer
getIntegerSize :: Nat -> Nat
getIntegerSize = getIntegerSizeHelper 1

{-
    size: size of the Integer
    num: reduced Integer
-}
getIntegerSizeHelper :: Nat -> Nat -> Nat
getIntegerSizeHelper size num
    | num < 10 = size
    | otherwise = getIntegerSizeHelper (size+1) (num `div` 10)

{-  
    Get the single digit form an Integer based on its index

    pos: the index of the Integer.
    eg 102
    pos 1: 2
    pos 2: 0
    pos 3: 1 
-}
getSingleDigit :: Nat -> Nat -> Nat
getSingleDigit pos dig = getSingleDigitHelper pos dig `mod` 10

getSingleDigitHelper :: Nat -> Nat -> Nat
getSingleDigitHelper 1 dig = dig
getSingleDigitHelper pos dig = getSingleDigitHelper (pos-1) dig `div` 10

{-
    Merge Sort:
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

{-
    Merge two lists (A,B): 
    compare each element in A with each element in list B 
-}
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

{-
    Pivot Sort: 
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
    10.1
    You are given two sorted arrays, A and B, 
    where A has a large enough buffer at the end to hold B. 
    Write a method to merge B into A in sorted order.
-}
sortedMerge :: Ord a => [a] -> [a] -> [a]
sortedMerge [] ys = ys
sortedMerge xs [] = xs
sortedMerge (x:xs) (y:ys)
    | x < y = x : sortedMerge xs (y:ys)
    | otherwise = y : sortedMerge (x:xs) ys

-- test case 
smArrX = [1, 4, 10, 100, 1000]
smArrY = [0, 0, 2, 1001]

{-
    10.2
    Giving an array of strings, 
    write a method to group all the anagrams
-}
groupAnagrams :: [String] -> [[String]]
groupAnagrams [] = []
groupAnagrams xs = gAHelper xs []

-- loop the array to build the result
gAHelper :: [String] -> [[String]]-> [[String]]
gAHelper [] ys = ys
gAHelper (x:xs) ys = gAHelper xs (gABuilder x ys)

-- give a new string, add it to the result [[String]]
gABuilder :: String -> [[String]]-> [[String]]
gABuilder x [] = [[x]]
gABuilder x (y:ys)
    | sort x == sort (head y) = (x:y):ys
    | otherwise = [y] ++ gABuilder x ys

gaArrX = ["eat","tea","tan","ate","nat","bat"]
gaArrY = [["bat"],["nat","tan"],["ate","eat","tea"]]

{-
    10.11
    In an array of integers, 
    a "peak" is an element which is >= the adjacent integers
    a "valley" is an element which is <= to the adjacent integers. 
    For example, in the array {5, 8, 6, 2, 3, 4, 6}, 
    {8, 6} are peaks and {5, 2} are valleys. 
    Given an array of integers, 
    sort the array into an alternating sequence of peaks and valleys

    Explain: it is better draw the plot in the graph.
    The peck and valley will show clear in the 2-D graph, 
    the index is on the x-axis, the value of index on the y-axis

    alternating sequence mean, valleys and pecks are alternating

    input: 5,3,1,4,2 
    peaks: 5, 4
    valley: 1, 2

    output: 5,1,4,2,3
    peaks: (5,4,3)
    valley: (1,2)
-}
data Movement = Peak | Valley | Start

instance Eq Movement where
    Peak == Peak = True
    Valley == Valley = True
    Start == Start = True
    Peak == Valley = False
    Peak == Start = False
    Valley == Peak = False
    Valley == Start = False
    Start == Peak = False
    Start == Valley = False

shufflePeakValley :: Ord a => [a] -> [a]
shufflePeakValley = shuffleHelper Start

shuffleHelper :: Ord a => Movement -> [a] -> [a]
shuffleHelper _ [] = []
shuffleHelper _ [x] = [x]
shuffleHelper Start (x1:x2:xs) 
    | x1 >= x2 = x1: shuffleHelper Valley (x2:xs)
    | otherwise = x1: shuffleHelper Peak (x2:xs)
shuffleHelper Peak (x1:x2:xs) 
    | x1 < x2 = x2: shuffleHelper Valley (x1:xs)
    | otherwise = x1: shuffleHelper Valley (x2:xs)
shuffleHelper Valley (x1:x2:xs)
    | x1 > x2 = x2: shuffleHelper Peak (x1:xs)
    | otherwise = x1: shuffleHelper Peak (x2:xs)

pvArray = [5, 3, 1, 2, 3]
