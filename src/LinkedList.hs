module LinkedList where

import Tree (BiTree(..), testTree2, createHeap)
import Basics (Nat)

-- A sequence of nodes, simple very of graph
data LinkedList a = Null'| Node' a (LinkedList a) deriving Show

instance Eq a => Eq (LinkedList a) where
  Null' == Null' = True
  Null' == (Node' _ _) = False
  (Node' _ _) == Null' = False
  (Node' x1 x2) == (Node' y1 y2) = x1 == y1 && x2 == y2

testLink = Node' 1 $ Node' 2 $ Node' 1 $ Node' 5 $ Node' 2 $ Node' 3 Null'

{-
    2.1
    Write code to remove duplicates from an unsorted linked list.

    Test Case:
        removeDup testLink
-}
removeDup :: Eq a => LinkedList a -> LinkedList a
removeDup xs = removeDupHelper xs []

removeDupHelper :: Eq a => LinkedList a -> [LinkedList a] -> LinkedList a
removeDupHelper Null' _ = Null'
removeDupHelper (Node' x y) dict
    | Node' x Null' `elem` dict = removeDupHelper y dict
    | otherwise = Node' x (removeDupHelper y (Node' x Null':dict))

-- (no dict)loop into the original list, and refine the result list
removeDupBF :: Eq a => LinkedList a -> LinkedList a
removeDupBF xs = removeDupBFHelper xs xs

removeDupBFHelper :: Eq a => LinkedList a -> LinkedList a -> LinkedList a
removeDupBFHelper Null' refined = refined
removeDupBFHelper _ Null' = Null'
removeDupBFHelper (Node' x l1) (Node' y l2) = 
    removeDupBFHelper l1 (refineLink (Node' x Null') (Node' y l2) True)

refineLink :: Eq a => LinkedList a -> LinkedList a -> Bool -> LinkedList a
refineLink _ Null' _ = Null'
refineLink x (Node' y l) isFirst
    | (x == Node' y Null') && not isFirst = refineLink x l isFirst
    | (x == Node' y Null') && isFirst = Node' y (refineLink x l False)
    | otherwise = Node' y (refineLink x l isFirst)

{-
    4.3 Given a binary tree, design an algorithm which creates 
    a linked list of all the nodes at each depth.
-}
listDepth :: Ord a => BiTree a -> [[a]]
listDepth root = divideTree (createHeap root)

-- Divide the complete tree base on depth into array of array
divideTree :: [Maybe a] -> [[a]]
divideTree = divideTreeHelper [] 1

divideTreeHelper :: [[a]] -> Nat -> [Maybe a] -> [[a]]
divideTreeHelper xs _ [] = xs
divideTreeHelper [] track (Nothing:ys) = error ""
divideTreeHelper [] track (Just y:ys) = divideTreeHelper [[y]] (track+1) ys
divideTreeHelper xs track (Nothing:ys) =
    if isPowerOfTwo track
        then divideTreeHelper (xs ++ [[]]) (track+1) ys
        else divideTreeHelper xs (track+1) ys
divideTreeHelper xs track (Just y:ys) =
    if isPowerOfTwo track
        then divideTreeHelper (xs ++ [[y]]) (track+1) ys
        else divideTreeHelper (unchange ++ lastOne) (track+1) ys
    where unchange = init xs
          lastOne = [last xs ++ [y]]

-- Check whether it is belong to the set of power of two
isPowerOfTwo :: Nat -> Bool
isPowerOfTwo 1 = True
isPowerOfTwo x
    | x `mod` 2 == 1 = False
    | otherwise = isPowerOfTwo (x `div` 2)
