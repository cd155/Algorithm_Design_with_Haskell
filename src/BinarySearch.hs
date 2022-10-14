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
binarySearch'' :: (Nat -> Nat) -> Nat -> (Nat, Nat) -> [Nat]
binarySearch'' f t = seek
    where seek (a, b) | a > b = []
                      | t < f m = seek(a, m-1)
                      | t == f m = [m]
                      | otherwise = seek(m+1, b)
                      where m = (a + b) `div` 2

{-
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
