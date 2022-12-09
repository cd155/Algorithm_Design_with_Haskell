module Recursion where

import Basics (Nat)


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
