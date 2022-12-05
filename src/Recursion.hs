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
fibMemo 0 = 0
fibMemo 1 = 1
fibMemo n = fibData!!n
    where fibData = fibDataHelper 0 n [0, 1]

-- Store fib sequence database from [0,1] to [0,1,...n]
fibDataHelper :: Nat -> Nat -> [Nat] -> [Nat]
fibDataHelper i n fibData 
    | i+1 == n = fibData 
    | otherwise = fibDataHelper (i+1) n 
        (fibData ++ [fibData!!i + fibData!!(i+1)])
