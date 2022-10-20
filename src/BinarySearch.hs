{-
    Divide and Conquer Algorithm
-}

module BinarySearch where

import Basics (Nat)
import Data.Bits (Bits(shiftR, (.&.)))

{-
    Given f and t, find x where t = f (x)
    x is in range of 0 to t
-}
linearSearch :: (Nat -> Nat) -> Nat -> [Nat]
linearSearch f t = [x | x <- [0..t ], t == f x]

linearSearch' :: (Nat -> Nat) -> Nat -> [Nat]
linearSearch' f t = seek (0, t)
    where seek (a, b) = [x | x <- [a..b], t == f x]

-- test function
expr2 :: Nat -> Nat
expr2 = (^) 2

constant2 :: Nat -> Nat
constant2 _ = 2

{-
    t is f (x)
    f (x) < f (m) => x < m
    f (x) = f (m) => x = m
    f (x) > f (m) => x > m
-}
binarySearch'' :: (Nat -> Nat) -> Nat -> (Nat, Nat) -> [Nat]
binarySearch'' f t = seek
    where seek (a, b) | a > b = []
                      | t < f m = seek(a, m-1)
                      | t == f m = [m]
                      | otherwise = seek(m+1, b)
                      where m = (a + b) `div` 2

{-
    10.3
    There is a sorted number array have been rotated with an unknown pivot.
    Give a target number, return its index in this array
    
    Original array:        [1, 3, 4, 5, 7, 10, 14, 15, 16, 19, 20, 25]
    Rotate at the index 6: [15, 16, 19, 20, 25, 1, 3, 4, 5, 7, 10, 14]

    four situations, index 0, index mid
    0: index 0 == target or index mid == target
    1: index 0 < target < index mid  => first half
    2: index mid < target < index 0  => second half
    3: target < index 0, target < index mid
        if index 0 > index mid: first half
        otherwise: second half
    4: target > index 0, target > index mid
        if index 0 > index mid: first half
        otherwise: second half  
-}

-- given a target and a rotated array, return the target's index
searchRotatedArray :: Nat -> [Nat] -> Nat
searchRotatedArray t xs = searchRAHelper t xs 0

{-
    t: target
    xs: rotated array
    index: tracking index

    Note: 3 and 4 can be combine into otherwise
        | t < head xs && t < xs !! mid = 
            if (head xs > xs !! mid) then searchRAHelper t (fst splitted) index 
            else searchRAHelper t (snd splitted) index+mid
        | t > head xs && t > xs !! mid = 
            if (head xs > xs !! mid) then searchRAHelper t (fst splitted) index 
            else searchRAHelper t (snd splitted) index+mid
-}
searchRAHelper :: Nat -> [Nat] -> Nat -> Nat
searchRAHelper _ [] index = index
searchRAHelper t [x] index
    | t == x = index + 1
    | otherwise = error "The number does not in this list."
searchRAHelper t xs index
    | t == head xs = index
    | t == xs !! mid = index + mid
    | t > head xs && t < xs !! mid = searchRAHelper t (fst splitted) index
    | t < head xs && t > xs !! mid = searchRAHelper t (snd splitted) index+mid
    | otherwise = if head xs > xs !! mid
        then searchRAHelper t (fst splitted) index
        else searchRAHelper t (snd splitted) index+mid
    where mid = length xs `div` 2
          splitted = splitAt mid xs

sraArrX :: [Nat]
sraArrX = [15, 16, 19, 20, 25, 1, 3, 4, 5, 7, 10, 14]

{-
    10.4
    Search in a sorted array, but you don't know the size of it, turn the target's index
    
    elementAt :: Nat -> [Nat]-> Nat
    return the element in the index, if index is out of bound return -1
-}
searchWithNoSize :: Nat -> [Nat] -> Nat
searchWithNoSize t xs = searchWithNoSizeHelper t xs range
    where range = findInterval t xs

-- binary search after get the interval
searchWithNoSizeHelper :: Nat -> [Nat] -> (Nat, Nat) -> Nat
searchWithNoSizeHelper t xs range
    | start == end = if elementAt start xs == t
        then start else error "The number does not in this list."
    | (t <= elementAt mid xs) || (elementAt mid xs == -1) =
        searchWithNoSizeHelper t xs (start, mid)
    | otherwise = searchWithNoSizeHelper t xs (mid+1, end)
    where start = fst range
          end = snd range
          mid = (start + end) `div` 2

{-
    t: target
    xs: the sorted list
-}
findInterval :: Nat -> [Nat] -> (Nat, Nat)
findInterval t xs = findIntervalHelper t xs 1

-- binary search find the interval by increment base on 2^k
findIntervalHelper :: Nat -> [Nat] -> Nat -> (Nat, Nat)
findIntervalHelper t xs step
    | elementAt 0 xs == t = (0, 0)
    | elementAt step xs == t = (step, step)
    | elementAt step xs == -1 = (step `div` 2, step) -- move up prevent an infinite loop
    | t < elementAt step xs = (step `div` 2, step)
    | t > elementAt step xs = findIntervalHelper t xs (step*2)
    | otherwise = error "not an exhaustive match" -- not sure if it is exhaustive match

-- fake the elementAt method by using length method
elementAt :: Nat -> [Nat]-> Nat
elementAt index xs
    | index >= length xs = -1
    | otherwise = xs !! index

{-
    10.5
    Given a sorted array of strings that is interspersed with empty strings, 
    write a method to find the location of a given string.
-}
sparseSearch :: String -> [String] -> Nat
sparseSearch "" xs = error "linear search for the first empty string."
sparseSearch t xs = sparseSearchHelper t xs (0, length xs - 1)

{-
    t: the target
    xs: the list
    range: the tracking range
-}
sparseSearchHelper :: String -> [String] -> (Nat, Nat) -> Nat
sparseSearchHelper t xs range
    | start == end = if xs !! start == t then start
        else error "The string does not in this list."
    | xs !! mid == "" =
        case linearSearchLeft t xs (start, mid) of
            Nothing -> sparseSearchHelper t xs (mid+1, end)
            Just True -> sparseSearchHelper t xs (start, mid)
            Just False -> sparseSearchHelper t xs (mid+1, end)
    | xs !! mid /= "" =
        case t <= xs !! mid of
            True -> sparseSearchHelper t xs (start, mid)
            False -> sparseSearchHelper t xs (mid+1, end)
    | otherwise = error "Other condition"
    where start = fst range
          end = snd range
          mid = (start + end) `div` 2


-- linear search to the left until find the indicator
linearSearchLeft :: String -> [String] -> (Nat, Nat) -> Maybe Bool
linearSearchLeft t xs range
    | end < start  = Nothing
    | (xs !! end /= "") && (t <= xs !! end ) = Just True -- left
    | (xs !! end /= "") && (t > xs !! end ) = Just False -- right
    | otherwise = linearSearchLeft t xs (start, end-1)
    where start = fst range
          end = snd range

ssArrX :: [String]
ssArrX = ["at", "", "", "", "ball", "", "", "car", "", "", "dad", "", ""]

{-
    10.6
    Imagine you have a 20 GB file with one string per line. 
    Explain how you would sort the file.

    Solution:
    divide the file into small trunks. 
    Sorted each chunks separately. 
    Merge chunks (similar to merge in merge sorting)
-}

{-
    min: length of string
    x: the value
-}
showBitVector :: Int -> Integer -> String
showBitVector min_ 0 = replicate min_ '0'
showBitVector min_ x = showBitVector (min_ - 1) (shiftR x 1) ++ show (x .&. 1)

{-
    10.7
    Given an input file with four billion non-negative integers, 
    provide an algorithm to generate an integer that is not contained in the file. 
    Assume you have 1 GB of memory available for this task.

    What if you only have 1 MB memory?
-}
type Bit = [Bool]

setBit :: Nat -> Bit -> Bit
setBit index xs = take index xs ++ True : drop (index+1) xs

setListBits :: [Nat] -> Bit -> Bit
setListBits [] ys = ys
setListBits (x:xs) ys = setListBits xs
    (if ys !! x then ys else setBit x ys)

genNumHelper :: Bit -> Nat -> Nat
genNumHelper [] track = track
genNumHelper (x:xs) track
    | x = genNumHelper xs (track+1) 
    | otherwise = track

genNum :: [Nat] -> Nat
genNum xs = genNumHelper (setListBits xs initalBit) 0
    where initalBit = replicate 10 False

{-
    10.8
    You have an array with all the numbers from 1 to N, where N is at most 32,000. 
    The array may have duplicate entries and you do not know what N is. 
    With only 4 kilobytes of memory available, 
    how would you print all duplicate elements in the array?

    Solution: 1 kb = 8000 bits
              4 kb = 32000 bits
    
    We can use a bit-liked structure to map all possible integer
    1. initialize a 32000 long bit ([Bool]) to all False
    2. if the value of index (given number) is false 
            set the value to its index with True
       else
            print the value
-}

{-
    10.9
    Given an M x N matrix in which each row and 
    each column is sorted in ascending order, 
    write a method to find an element.
-}
