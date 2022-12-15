module Recursion where

import Basics (Nat)
import Tree (findAllPaths)

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

    test case: allPaths (0,0) (3,3) [(1,2), (3,2)]
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
