module Tree where

import Basics (Nat)

-- A tree contains zero or more trees
data Tree a = Null1| Node1 a [Tree a] deriving Show

-- A binary tree contain max two trees
data BiTree a = Null | Node a (BiTree a) (BiTree a) deriving Show

{-
    What we defined here is a Direct Tree. (linked internally)

    Binary Search Tree: 
    all left trees (values) <= Node n < all right trees (values)

    Balanced Tree: 
    the height of left and right subtrees of every node differ <= 1

    Full tree: 
    every node other than the leaves has two children.

    Complete tree:
    every level, except the last, is completely filled
    and the last level is filled from left to right

    Perfect Trees: 
    all nodes have two children 
    and all leaves have the same level.
-}

-- Visit order: the left branch, the mid node, and the right branch
inOrderTraverse :: BiTree a -> [a]
inOrderTraverse Null = [] 
inOrderTraverse (Node n Null Null) = [n]
inOrderTraverse (Node n left Null) = inOrderTraverse left ++ [n]
inOrderTraverse (Node n Null right) = n: inOrderTraverse right
inOrderTraverse (Node n left right) =
    inOrderTraverse left ++ [n] ++ inOrderTraverse right

-- Visit order: the mid node, the left branch, and the right branch 
preOrderTraverse :: BiTree a -> [a]
preOrderTraverse Null = [] 
preOrderTraverse (Node n Null Null) = [n]
preOrderTraverse (Node n left Null) = n: preOrderTraverse left
preOrderTraverse (Node n Null right) = n: preOrderTraverse right
preOrderTraverse (Node n left right) =
    n: preOrderTraverse left ++ preOrderTraverse right

-- Visit order: the left branch, the right branch and the mid node, 
postOrderTraverse :: BiTree a -> [a]
postOrderTraverse Null = [] 
postOrderTraverse (Node n Null Null) = [n]
postOrderTraverse (Node n left Null) = postOrderTraverse left ++ [n]
postOrderTraverse (Node n Null right) = postOrderTraverse right ++ [n]
postOrderTraverse (Node n left right) =
    postOrderTraverse left ++ postOrderTraverse right ++ [n]

testTree = 
    Node 10 
        (Node 5 
            (Node 3 Null Null) (Node 7 Null Null)) 
        (Node 20 
            Null (Node 30 Null Null))

{-
    The (binary) heap data structure 
    is an array object that we can view as a nearly complete binary tree 

    Min Heaps: each node is smaller than its children
    Max Heaps: each node is greater than its children

    It would be better to implement a heap with an array.
    Using tree structure require more information share in the same level
-} 

testHeap = [10, 5, 20, 3, 7, 30]

-- Append the element to the end of the list
insert :: a -> [a] -> [a]
insert x xs = xs ++ [x]

viewAsTree :: [a] -> BiTree a
viewAsTree = viewAsTreeHelper 0

viewAsTreeHelper :: Nat -> [a] -> BiTree a
viewAsTreeHelper _ [] = Null
viewAsTreeHelper track xs
    | track < size = 
        Node (xs!!track) (viewAsTreeHelper leftTrack xs) (viewAsTreeHelper rightTrack xs)
    | otherwise = Null
    where size = length xs
          (leftTrack, rightTrack) = 
            if track == 0 then (1 ,2) else (2*track + 1, 2*track + 2)

-- maxHeapify :: [a] -> [a]
