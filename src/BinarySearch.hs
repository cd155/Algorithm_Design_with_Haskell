{-
    Divide and Conquer Algorithm
-}

module BinarySearch where

import Basics (Nat)

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
linearSearch'' :: (Nat -> Nat) -> Nat -> (Nat, Nat) -> [Nat]
linearSearch'' f t = seek
    where seek (a, b) | a > b = []
                      | t < f m = seek(a, m-1)
                      | t == f m = [m]
                      | otherwise = seek(m+1, b)
                      where m = (a + b) `div` 2
