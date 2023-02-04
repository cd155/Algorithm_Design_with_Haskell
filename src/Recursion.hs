module Recursion where

import Basics (Nat)
import Tree (findAllPaths)
import qualified Data.Vector as V

{-
    Recursion: the program call itself

    Dynamic Programming: divide the problem into sub problem, solving sub problem
    will result in solving the whole problem

    Memoization: store the result of sub problems for the future use
-}

{-
    Finding fibonacci sequence can be solved by find the fibonacci number of its 
    sub-program. Use recursion method without memoization.
-}
fib :: Nat -> Nat
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Use memoization to store all the fib sequence in an array.
fibMemo :: Nat -> Nat
fibMemo n = fibData!!n
    where fibData = fibDataHelper 0 n [0, 1]

-- Store fib sequence database from [0,1] to [0,1,...n]
fibDataHelper :: Nat -> Nat -> [Nat] -> [Nat]
fibDataHelper i n fibData
    | i+1 == n = fibData
    | otherwise = fibDataHelper (i+1) n
        (fibData ++ [fibData!!i + fibData!!(i+1)])

{-
    8.1
    A child is running up a staircase with n steps and can hop either 
    1 step, 2 steps, or 3 steps at a time. Implement a method to 
    count how many possible ways the child can run up the stairs.
-}
countWays :: Nat -> Nat
countWays 0 = 1
countWays n
    | n < 0 = 0
    | otherwise = countWays (n-3) + countWays (n-2) + countWays (n-1)

-- Memoization version
countWaysMemo :: Nat -> Nat
countWaysMemo n = countWaysData!!n
    where countWaysData = countWaysDataHelper 0 n [1,1,2]

countWaysDataHelper :: Nat -> Nat-> [Nat] -> [Nat]
countWaysDataHelper i n countData
    | i+2 == n = countData
    | otherwise = countWaysDataHelper (i+1) n (countData ++
        [countData!!i + countData!!(i+1) + countData!!(i+2)])

{-
    8.1 extension: find all possible ways the child can run up the 
    stairs
    
    test case: findValidSeq 5 [1,2]
-}
findValidSeq :: Nat -> [Nat] -> [[Nat]]
findValidSeq stair steps = findValidSeqHelper stair steps [[]]

{-
    existSeq works like a stack, once we know the result meet
    the requirement, we put it as the part of answer, then we 
    update the stack with the potential sequence for the next run
-}
findValidSeqHelper :: Nat -> [Nat] -> [[Nat]] -> [[Nat]]
findValidSeqHelper stair steps existSeq
    | null nextOri = ans
    | otherwise = ans ++ findValidSeqHelper stair steps nextOri
    where updateOri = permutateSeq existSeq steps
          -- find sequences satisfy requirement
          ans = filter (\x -> sum x == stair) updateOri
          -- find potential sequences for the next run
          nextOri = filter (\x -> sum x < stair) updateOri

{-- 
    Give a list of sequence and potential appending, and create all 
    possible permutations.
--}
permutateSeq :: [[Nat]] -> [Nat] -> [[Nat]]
permutateSeq existSeq [] = []
permutateSeq existSeq (x:xs) = map (++ [x]) existSeq ++
                               permutateSeq existSeq xs

{-
    8.2
    Imagine a robot sitting on the upper left corner of grid with r rows 
    and c columns. The robot can only move in two directions, right and 
    down, but certain cells are "off limits" such that the robot cannot 
    step on them. Design an algorithm to find a path for the robot from 
    the top left to the bottom right.

    Let assume the start point is (0,0), the a normal x-axis, but a 
    opposite y-axis (the bottom half is positive), each time we either move 
    right or move down, this mean each time, we increase (x,y) either x or y 
    by one until we reach to (c, r) 

    Theoretically, it has 2^(c+r) different paths

    1. find all valid paths
       test case: allPaths (0,0) (2,2) [(1,1), (1,2)]
    
    2. find only one valid path
       test case: findOnePath (0,0) (3,3) [(1,1), (2,1), (3,1),(1,2),(1,3)]
-}

{-
    Find the next possible move

    (x, y): current position
    (c, r): final position
    constrs: cells are not available (can't include the start cell)
-}
move :: (Nat, Nat) -> (Nat, Nat) -> [(Nat, Nat)]-> [Maybe (Nat, Nat)]
move (x, y) (c, r) constrs
    | x+1 <= c && y+1 <= r && xCell `notElem` constrs && yCell `notElem` constrs =
        [Just xCell, Just yCell]
    | x+1 <= c && y+1 <= r && xCell `elem` constrs && yCell `notElem` constrs =
        [Nothing, Just yCell]
    | x+1 <= c && y+1 <= r && xCell `notElem` constrs && yCell `elem` constrs =
        [Just xCell, Nothing]
    | x+1 <= c && y+1 <= r && xCell `elem` constrs && yCell `elem` constrs =
        [Nothing, Nothing]
    | x+1 > c && y+1 <= r && yCell `notElem` constrs =
        [Nothing, Just yCell]
    | x+1 <= c && y+1 > r && xCell `notElem` constrs =
        [Just xCell, Nothing]
    | otherwise = [Nothing, Nothing]
    where xCell = (x+1, y)
          yCell = (x, y+1)

-- Check whether the cell is valid
isValidCell :: [(Nat, Nat)] -> [(Nat, Nat)] -> Bool
isValidCell [] _ = True
isValidCell _ [] = True
isValidCell (x:xs) constrs = (x `notElem` constrs) && isValidCell xs constrs

-- Create a heap to record all possible sequence
allPathsHelper ::
    [Maybe (Nat, Nat)] -> (Nat, Nat) -> [(Nat, Nat)]-> [Maybe(Nat, Nat)]
allPathsHelper [] _ _  = []
allPathsHelper (Nothing:xs) final constrs
    -- end infinite nothing loop
    | all (== Nothing) (Nothing:xs) = []
    | otherwise = nothingMoves ++ allPathsHelper (xs++nothingMoves) final constrs
    where nothingMoves = [Nothing, Nothing]
allPathsHelper (Just (x, y):xs) final constrs  =
    nextMoves ++ allPathsHelper (xs++nextMoves) final constrs
    where nextMoves = move (x,y) final constrs

-- Return the Completed Heap
allPaths :: (Nat, Nat) -> (Nat, Nat) -> [(Nat, Nat)] -> [Maybe(Nat, Nat)]
allPaths start end constrs =
            Just start: removeNothing (allPathsHelper [Just start] end constrs)

-- Return all unique paths index in a Heap
allPaths' :: (Nat, Nat) -> (Nat, Nat) -> [(Nat, Nat)] -> [[Nat]]
allPaths' start end constrs = findAllPaths completedHeap [] (length completedHeap-1)
    where completedHeap =
            Just start: removeNothing (allPathsHelper [Just start] end constrs)

-- Remove Nothing on the tail
removeNothing :: [Maybe(Nat, Nat)] -> [Maybe(Nat, Nat)]
removeNothing xs
    | xs!!(length xs -1) == Nothing = removeNothing $ init xs
    | otherwise = xs

-- 8.2, find only one valid path
isValidPath :: (Nat, Nat) -> (Nat, Nat) -> [(Nat, Nat)] -> Bool
isValidPath (x, y) (c, r) constrs
    | x == c && y == r = True
    | x > c || y > r = False
    | (x, y) `elem` constrs = False
    | otherwise = isValidPath (x+1, y) (c, r) constrs ||
                  isValidPath (x, y+1) (c, r) constrs

findOnePath :: (Nat, Nat) -> (Nat, Nat) -> [(Nat, Nat)] -> [(Nat, Nat)]
findOnePath (x, y) (c, r) constrs
    | isValidPath (x+1, y) (c, r) constrs =
        (x+1, y): findOnePath(x+1, y) (c,r) constrs
    | isValidPath (x, y+1) (c, r) constrs =
        (x, y+1): findOnePath(x, y+1) (c,r) constrs
    | otherwise = []

{-
    8.3
    A magic index in an array A [0 ... n-1] is defined to be an index such 
    that A[i] = i. Given a sorted array of distinct integers, write a 
    method to find a magic index, if one exists, in array A.

    For example:
    |0    |1    |2    |3    |4    |5    |6
    |-40  |-20  |0    |1    |4    |10   |12

    Solution 1: brutal force
    Solution 2: binary search 
-}

findMagicIndex :: [Int] -> Nat -> Int
findMagicIndex [] _ = error "No magic index in this array."
findMagicIndex arr preInd
    | arr !! mid == (preInd + mid) = preInd + mid
    | arr !! mid > (preInd + mid) = findMagicIndex fstHalf preInd
    | otherwise = findMagicIndex sndHalf (preInd+mid)
    where mid = length arr `div` 2
          (fstHalf, sndHalf) = splitAt mid arr

{-
    8.3 with a sorted array of non-distinct integers
    |0    |1    |2    |3    |4    |5    |6    |7    |8    |9    |10
    |-10  |-5   |2    |2    |2    |3    |4    |7    |9    |12   |13
-}

{-
    8.4
    Write a method to return all subsets of a set.

    input: a = [1,3,4,6,10]
    output: the power set of a

    test case = powerSetOf [3,3,7,10,1,15]
-}
powerSetOf :: [a] -> [[a]]
powerSetOf xs = map (map (xs!!)) resultInInd
    where resultInInd = powerSetHelper initalSeq (length xs) 0
          initalSeq = powerSetOfInd 0 (length xs)

-- Initialize the list of index for pairing
-- It is better to use index for paring because the index is a unique ID
powerSetOfInd :: Nat -> Nat -> [[Nat]]
powerSetOfInd i max
    | i < max = [i]: powerSetOfInd (i+1) max
    | otherwise = []

{-
    Copy the current set, and accumulate with new set

    inp: give list of seq, 
    len: length of original set, 
    count: count number (start with 0) indicate when to stop the loop
-}
powerSetHelper :: [[Nat]] -> Nat -> Nat -> [[Nat]]
powerSetHelper inp len count
    | count < len = inp ++ powerSetHelper newInp len (count+1)
    | otherwise = []
    where newInp = newPair inp len

-- Accumulate all new pair in the list
newPair :: [[Nat]] -> Nat -> [[Nat]]
newPair [] len = []
newPair (x:xs) len =
    newPairHelper x [(last x +1) .. len-1] ++ newPair (xs) len

-- Give a new seq append new possible pairing to it
newPairHelper :: [Nat] -> [Nat] -> [[Nat]]
newPairHelper _ [] = []
newPairHelper a (x:xs) = (a ++ [x]):  newPairHelper a xs

{-
    8.6
    Towers of Hanoi: 
    In the classic problem of the Towers of Hanoi, you have 3 towers and 
    N disks of different sizes which can slide onto any tower. The puzzle 
    starts with disks sorted in ascending order of size from top to bottom 
    (i.e., each disk sits on top of an even larger one). 
    
    You have the following constraints:
    1. Only one disk can be moved at a time.
    2. A disk is moved from the top of one tower to another tower.
    3. A disk cannot be placed on top of a smaller disk.

    Write a program to move the disks from the first tower to the last 
    using stacks.
-}
type Peg = String
type Move = (Peg, Peg)

{-
    inputs:
    number fo discs
    three peg name

    output: move sequence

    Rationale:
    1. move n-1 stack from start peg to a temp peg,
    2. move start peg to the end peg
    3. move n-1 stack from temp peg to end peg
-}
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _          = []
hanoi 1 start _ end    = [(start, end)]
hanoi n start temp end =
        hanoi (n-1) start end temp ++ [(start, end)] ++ hanoi (n-1) temp start end

{-
    8.7 and 8.8
    1. Generate all permutations of a unique string

    2. Generate all permutations of an non-unique string

    test case: (sort $ permutations "abc") == (sort $ genPerm "abc")
    
    bottom-up solution p356
-}
genPerm :: String -> [String]
genPerm chars = genPermHelper [[]] listStr
    where listStr = map (: []) chars

-- Loop through all possible characters/strings
genPermHelper :: [String] -> [String] -> [String]
genPermHelper inp [] = inp
genPermHelper inp (x:xs) = genPermHelper newInp xs
    where newInp = appendPerm inp x []

-- Accumulate new permutations base on the string (not finished)
appendPerm :: [String] -> String -> [String] -> [String]
appendPerm [] _ acc = acc
appendPerm (x:xs) c acc = appendPerm xs c (acc++newPerm)
    where -- newPerm = complement acc (appendPermHelper x c 0) -- with duplicates
          newPerm = complement acc (appendPermHelper' x c 0 []) -- without duplicates

-- another check for duplicates in appendPerm level, 
-- this need to work with appendPermHelper'
complement :: [String] -> [String] -> [String]
complement _ [] = []
complement a (x:xs)
    | x `elem` a = complement a xs
    | otherwise = x: complement a xs

-- Insert the character/string to every index of the input
-- appendPermHelper :: String -> String -> Nat -> [String]
-- appendPermHelper str c ind
--     | ind > length str = []
--     | otherwise = (fstHalf ++ c ++ sndHalf): appendPermHelper str c (ind+1)
--     where (fstHalf,sndHalf) = splitAt ind str

-- Insert the character/string to every index of the input with no duplicates
appendPermHelper' :: String -> String -> Nat -> [String] -> [String]
appendPermHelper' str c ind acc
    | ind > length str = acc
    | newPerm `elem` acc = appendPermHelper' str c (ind+1) acc
    | otherwise = appendPermHelper' str c (ind+1) (newPerm:acc)
    where (fstHalf,sndHalf) = splitAt ind str
          newPerm = fstHalf ++ c ++ sndHalf

{-
    8.9
    Implement an algorithm to print all valid (eg: properly opened and 
    closed) combinations of n pairs of parentheses.

    test case: genPerm' [paren,paren,paren]

    modify the genPerm from 8.7 and 8.8 to make it works for string, 
    and remove duplicates in two different levels.
-}
paren :: String
paren = "()"

-- Generate all possible valid parentheses
genPerm' :: [String] -> [String]
genPerm' = genPermHelper [[]]

{-
    8.10
    Implement the "paint fill" function that one might see on many image 
    editing programs. That is, given a screen (represented by a 
    two-dimensional array of colors), a point, and a new color, fill in 
    the surrounding area until the color changes from the original color.

    test case: fillUpColor image (0,0) Blue
    
    original image:
    [
        [Red,Red,Red],
        [Red,Yellow,Red],
        [Yellow,Yellow,Blue]
    ]

    expect image:
    [
        [Blue,Blue,Blue],
        [Blue,Yellow,Blue],
        [Yellow,Yellow,Blue]
    ]
-}
data Color = Red | Yellow | Blue deriving Show

instance Eq Color where
    Red == Red = True
    Red == Yellow = False
    Red == Blue = False
    Yellow == Yellow = True
    Yellow == Blue = False
    Yellow == Red = False
    Blue == Blue = True
    Blue == Yellow = False
    Blue == Red = False

data Direction = Up | Down | PLeft | PRight

type Image = V.Vector (V.Vector Color)

-- test case
image :: Image
image = V.fromList
    [
        V.fromList [Red,    Red,    Red],
        V.fromList [Red,    Yellow, Red],
        V.fromList [Yellow, Yellow, Blue]
    ]

-- fill up a color 
fillUpColor :: Image -> (Int, Int) -> Color -> Image
fillUpColor img (i,j) c = foldl (\acc x -> paint acc x c ) img pList
    where pList = findArea img (i,j)

-- Paint a color in one location
paint :: Image -> (Int, Int) -> Color -> Image
paint vs (i,j) c  =
    fstHVects V.++ V.fromList[newPaintRow] V.++ V.drop 1 secHVects
    where
        (fstHVects, secHVects) = V.splitAt i vs
        (fstHPaintRow, secHPaintRow) = V.splitAt j (vs V.! i)
        newPaintRow =
            fstHPaintRow V.++ V.fromList[c] V.++ V.drop 1 secHPaintRow

-- Find all locations which need to paint
findArea :: Image -> (Int, Int) -> [(Int, Int)]
findArea img (i,j) = uniq (
    (i,j):
    findAreaOnDir img (i,j) boundC Up ++
    findAreaOnDir img (i,j) boundC Down ++
    findAreaOnDir img (i,j) boundC PLeft ++
    findAreaOnDir img (i,j) boundC PRight) []
    where boundC = img V.! i V.! j

-- remove duplicates
uniq :: [(Int, Int)] -> [(Int, Int)]-> [(Int, Int)]
uniq [] buf = buf
uniq (x:xs) buf
    | x `elem` buf = uniq xs buf
    | otherwise = uniq xs (x:buf)

-- find potential position by direction
findAreaOnDir :: Image -> (Int, Int) -> Color -> Direction -> [(Int, Int)]
findAreaOnDir img (i,j) c Up
    | isInBoundAndSameColor img (i,j-1) c =
        (i,j-1): findAreaOnDir img (i,j-1) c PLeft
    | isInBoundAndSameColor img (i-1,j) c =
        (i-1,j): findAreaOnDir img (i-1,j) c Up
    | isInBoundAndSameColor img (i,j+1) c =
        (i,j+1): findAreaOnDir img (i,j+1) c PRight
    | otherwise = []
findAreaOnDir img (i,j) c Down
    | isInBoundAndSameColor img (i,j-1) c =
        (i,j-1): findAreaOnDir img (i,j-1) c PLeft
    | isInBoundAndSameColor img (i+1,j) c =
        (i+1,j): findAreaOnDir img (i+1,j) c Down
    | isInBoundAndSameColor img (i,j+1) c =
        (i,j+1): findAreaOnDir img (i,j+1) c PRight
    | otherwise = []
findAreaOnDir img (i,j) c PLeft
    | isInBoundAndSameColor img (i-1,j) c =
        (i-1,j): findAreaOnDir img (i-1,j) c Up
    | isInBoundAndSameColor img (i,j-1) c =
        (i,j-1): findAreaOnDir img (i,j-1) c PLeft
    | isInBoundAndSameColor img (i+1,j) c =
        (i+1,j): findAreaOnDir img (i+1,j) c Down
    | otherwise = []
findAreaOnDir img (i,j) c PRight
    | isInBoundAndSameColor img (i-1,j) c =
        (i-1,j): findAreaOnDir img (i-1,j) c Up
    | isInBoundAndSameColor img (i,j+1) c =
        (i,j+1): findAreaOnDir img (i,j+1) c PRight
    | isInBoundAndSameColor img (i+1,j) c =
        (i+1,j): findAreaOnDir img (i+1,j) c Down
    | otherwise = []

-- condition determine potential fill up position
isInBoundAndSameColor :: Image -> (Int, Int) -> Color -> Bool
isInBoundAndSameColor img (i,j) c = isInBound img (i,j) && selectC == c
    where selectC = img V.! i V.! j

-- check if position if in bound
isInBound :: Image -> (Int, Int) -> Bool
isInBound img (i,j)
    | (0 <= i && i < xBound) && (0 <= j && j < yBound) = True
    | otherwise = False
    where xBound = length img
          yBound = length $ img V.! 0
