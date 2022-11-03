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

-- View the array as a heap structure
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

-- Given an array, create a Max-Heap 
buildMaxHeap :: Ord a => [a] -> [a]
buildMaxHeap xs = buildMaxHeapHelper xs (length xs `div` 2)

buildMaxHeapHelper :: Ord a => [a] -> Nat -> [a]
buildMaxHeapHelper xs index 
    | index < 0 = xs
    | otherwise = buildMaxHeapHelper (maxHeapify xs index) (index-1)

-- Given an Array and the index of the node, perform maxHeapify
maxHeapify :: Ord a => [a] -> Nat -> [a]
maxHeapify xs index
    | largestIndex /= index = 
        -- swap index with largest, then repeat maxHeapify
        maxHeapify (swapTwoInList index largestIndex xs) largestIndex  
    | otherwise = xs
    where largestIndex = maxInThree xs index leftTrack rightTrack
          (leftTrack, rightTrack) =
            if index == 0 then (1 ,2) else (2*index + 1, 2*index + 2)

-- Find the largest index base on the value on three elements
maxInThree :: Ord a => [a] -> Nat -> Nat -> Nat -> Nat
maxInThree xs parent left right
    | left >= length xs = parent
    | left == (length xs - 1) = 
        if xs!!parent >= xs!!left then parent else left
    | xs!!parent >= xs!!left && xs!!parent >= xs!!right = parent
    | xs!!left >= xs!!parent && xs!!left >= xs!!right = left
    | otherwise = right

-- Given two indexes and an list, swap the values in the list
swapTwoInList :: Nat -> Nat -> [a] -> [a]
swapTwoInList _ _ [] = []
swapTwoInList i j xs
    | i < j = firstHalf1 ++ [xs!!j] ++
        take (j-i-1) (tail secondHalf1) ++ [xs!!i] ++
        drop (j-i) (tail secondHalf1)
    | i > j = firstHalf2 ++ [xs!!i] ++
        take (i-j-1) (tail secondHalf2) ++ [xs!!j] ++
        drop (i-j) (tail secondHalf2)
    |otherwise = xs
    where (firstHalf1, secondHalf1) = splitAt i xs
          (firstHalf2, secondHalf2) = splitAt j xs

{-
    Heap Sort algorithm
    1. create a max-heap
    2. swap the first element(the largest) with the last element
    3. lock the last element, update the new heap, perform max-heap on new heap
    4. repeat step 2 until the list has one or less elements
-}
heapSort :: Ord a => [a] -> [a]
heapSort xs = heapSortHelper (buildMaxHeap xs)

heapSortHelper :: Ord a => [a] -> [a]
heapSortHelper [] = [] 
heapSortHelper [x] = [x] 
heapSortHelper (x:xs) = heapSortHelper (maxHeapify swapArr 0) ++ [x]
    where swapArr = last xs : init xs

{-
    Depth-first search: starts at the root node
        explores as far as possible along each branch before backtracking.
        https://en.wikipedia.org/wiki/Depth-first_search

    Breadth-first search: 
        starts at the tree root and explores all nodes at the present depth 
        prior to moving on to the nodes at the next depth level.
        https://en.wikipedia.org/wiki/Breadth-first_search
-}

-- Depth-first search to find the path in a Tree
findPath :: Ord a => a -> BiTree a -> [a]
findPath t Null = []
findPath t (Node n Null Null) = if n == t then [n] else []
findPath t (Node n left Null)
    | t == n = [n]
    | otherwise = if null (findPath t left) then [] else n: findPath t left
findPath t (Node n Null right)
    | t == n = [n]
    | otherwise = if null (findPath t right) then [] else n: findPath t right
findPath t (Node n left right)
    | t == n = [n]
    | otherwise =
        if null (findPath t left) && null (findPath t right)
        then [] else n: findPath t left ++ findPath t right

-- -- add a series of appendList together
-- findPathHelper :: [[a]] -> [[a]]-> [[a]]
-- findPathHelper xs [] = xs
-- findPathHelper [] _ = error "The start root cannot be empty."
-- findPathHelper (x:xs) (y:ys) = appendList x y ++ findPathHelper xs ys

-- {-
--     create all possible combination
--     Given a prefix, and a list of appending element,
--     create a list contain a possible combination.
--     for example, 
--     prefix: [5,4,10]
--     appending list: [4,1]
--     answer: [[5,4,10,4], [5,4,10,1]]
-- -} 
-- appendList :: [a] -> [a] -> [[a]]
-- appendList _ [] = [] -- finished appending
-- appendList prefix (y:ys) = (prefix ++ [y]) : appendList prefix ys

{-
    we can create a completed heap by using Breadth-first visit
    then, we can trace back the path.
-}
createACompletedHeap :: [BiTree a] -> [Maybe a]
createACompletedHeap [] = []
createACompletedHeap [Null] = []
createACompletedHeap (Null: xs) = Nothing: createACompletedHeap xs
createACompletedHeap (Node n left right:xs) = 
    Just n: createACompletedHeap (xs ++ [left, right])
