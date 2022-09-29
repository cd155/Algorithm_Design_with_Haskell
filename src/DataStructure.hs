module DataStructure where

import Basics (Nat)
import Data.Array

{-
    Symmetric lists
    (xs, ys) represents the standard list xs ++ reverse ys

    cons: prepend
    snoc: append
-}
type SymList a = ([a], [a])

-- convert to the standard list
fromSL :: SymList a -> [a]
fromSL (xs, ys) = xs ++ reverse ys


{-
    two invariants:
    null xs ⇒ null ys ∨ single ys
    null ys ⇒ null xs ∨ single xs
-}
snocSL :: a -> SymList a -> SymList a
snocSL x (xs, ys) = if null xs then (ys, [x]) else (xs, x : ys)

lastSL :: SymList a -> a
lastSL (xs, ys) = if null ys then head xs else head ys

tailSL :: SymList a -> SymList a
tailSL (xs, ys)
    | null xs = if null ys then undefined else ([], [])
    | single xs = (reverse bs, as)
    | otherwise = (tail xs, ys)
    where (as, bs) = splitAt (length ys `div` 2) ys

single :: [a] -> Bool
single []     = False
single [x]    = True
single (x:xs) = False

{-
    Random-access lists
-}
fetch :: Nat -> [a] -> a
fetch k xs = if k == 0 then head xs else fetch (k - 1) (tail xs)

{-
    tree with no size
    data Tree a = Leaf a | Node (Tree a) (Tree a)

    size :: Tree a -> Nat
    size (Leaf x) = 1
    size (Node t1 t2 ) = size t1 + size t2
-}
data Tree a = Leaf a | Node Nat (Tree a) (Tree a)

size :: Tree a -> Nat
size (Leaf _) = 1
size (Node n _ _ ) = n

node :: Tree a -> Tree a -> Tree a
node t1 t2 = Node (size t1 + size t2) t1 t2

data Digit a = Zero | One (Tree a)
type RAList a = [Digit a]

fromRA :: RAList a -> [a]
fromRA = concatMap from
    where from Zero = [ ]
          from (One t) = fromT t

fromT :: Tree a -> [a]
fromT (Leaf x) = [x]
fromT (Node _ t1 t2 ) = fromT t1 ++ fromT t2

{-
    Arrays
-}
