module Basics where

{-
    1.1 Basic types and functions
-}

type Nat = Int

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x : xs) = if p x then x : filter' p xs else filter' p xs

-- Example
-- foldr (⊕) e [x, y, z] = x ⊕ (y ⊕ (z ⊕ e))
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f e [] = e
foldr' f e (x:xs) = f x (foldr' f e xs)

label' :: [a] -> [(Nat, a)]
label' xs = zip [0 ..] xs

length' :: [a] -> Nat
length' xs = foldr ($++) 0 xs
    where ($++) _ n = n + 1

-- Example:
-- foldl (⊕) e [x, y, z] = ((e ⊕ x) ⊕ y) ⊕ z
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f e [] = e
foldl' f e (x : xs) = foldl' f (f e x) xs

length :: [a] -> Nat
length = foldl ($++) 0 
    where ($++) n x = n + 1

foldl'' :: (c -> b -> c) -> c -> [b] -> c
foldl'' f e xs = ((foldr' (flip' f ) e ) . reverse) xs

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

{-
    1.2 Processing lists
-}

-- Example: 
-- scanl (⊕) e [x, y, z] = [e, e ⊕ x, (e ⊕ x) ⊕ y, ((e ⊕ x) ⊕ y) ⊕ z]
scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' f e [] = [e]
scanl' f e (x : xs) = e : scanl' f (f e x) xs

{-
    1.3 Inductive and recursive definitions
-}


perms' :: [a] -> [[a]]
perms' [] = [[]]
-- List comprehension 
-- [x*2 | x <- [1..10], x*2 >= 12]
-- [x*y | x <- [2,5,10], y <- [8,10,11]]
perms' (x:xs) = [zs | ys <- perms' xs, zs <- inserts x ys]

-- Example: 
-- inserts 1 [2, 3] = [[1, 2, 3], [2, 1, 3], [2, 3, 1]]
inserts :: a -> [a] -> [[a]]
inserts x [] = [[x]]
inserts x (y:ys) = (x:y:ys) : map (y:)(inserts x ys)
