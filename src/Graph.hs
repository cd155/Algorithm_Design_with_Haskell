module Graph where

import Basics (Nat)
import Tree (BiTree(..), testHeap, viewAsTree)

-- representing Graph
type Graph = ([Vertex], [Edge])
type Edge = (Vertex, Vertex, Weight)
type Vertex = Nat -- represent by node one, node two...
type Weight = Int

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
