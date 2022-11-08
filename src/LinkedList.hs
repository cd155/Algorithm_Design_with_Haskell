module LinkedList where

import Tree (BiTree(..))
import Basics (Nat)

-- A sequence of nodes, simple very of graph
data LinkedList a = Null'| Node' a (LinkedList a) deriving Show

{-
    4.3 Given a binary tree, design an algorithm which creates 
    a linked list of all the nodes at each depth.
-}
listDepth :: Ord a => BiTree a -> [[a]]
listDepth root = divideTree (listDepthHelper [root])

-- Fill in the binary binary tree with Nothing to make it completed
listDepthHelper :: Ord a => [BiTree a] -> [Maybe a]
listDepthHelper [] = []
listDepthHelper [Null] = []
listDepthHelper (Null: xs) =
    if null list then [] else Nothing: listDepthHelper (xs ++ [Null, Null])
        where list = [x | x <- xs, x /= Null]
listDepthHelper (Node n left right:xs) = Just n: listDepthHelper (xs ++ [left, right])

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

-- test case
testTree =
    Node 1
        (Node 7
            (Node 2 Null Null)
            (Node 6
                (Node 5 Null Null)
                (Node 11 Null Null)))
        (Node 9
            Null
            (Node 9
                (Node 5 Null Null)
                Null))
