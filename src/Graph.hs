module Graph where

import Basics (Nat)
import Data.Map (Map, fromList, member, insert, empty, lookup)
import Prelude hiding (lookup)
import Data.Maybe (isNothing, fromJust)

-- representing Graph
type Graph = ([Vertex], [Edge])
type Edge = (Vertex, Vertex, Weight)
type Vertex = Nat -- represent by node one, node two...
type Weight = Float

-- Giving a graph and a vertex, find all edges that start with this vertex
findEdges :: Graph -> Vertex -> [Edge]
findEdges g v =
    [(start, end, weight)| (start, end, weight) <- allEdges, start == v ]
        where allEdges = snd g

testGraph :: Graph
testGraph = ([0, 1, 2, 3, 4, 5],
    [
        (0, 5, 0.5),
        (0, 1, 0.4),
        (0, 4, 0.1),
        (1, 4, 0.5),
        (1, 3, 0.2),
        (2, 1, 0.5),
        (3, 2, 0.8),
        (3, 4, 0.3)
    ])

initMap :: Map Vertex Bool
initMap = fromList [(0, False), (1, False), (2, False), (3, False), (4, False), (5, False)]

{-
    4.1: Given a directed graph, design an algorithm to find out 
    whether there is a route between two nodes.

    Giving a start vertex and an end vertex, find if the path exists
-}
isPathExist :: Graph -> Vertex -> Vertex -> Bool
isPathExist g start end = if possEdges == [] then False else True
    where possEdges = findAllPaths g (findEdges g start) end initMap

{-
    Giving a list of potential edge, search the target vertex
    
    The search algorithm is base on Breadth-first.
    We using a stack to check every possible nodes in this level, 
    then move to the next level.
-}
findAllPaths :: Graph -> [Edge] -> Vertex -> Map Vertex Bool -> [Edge]
findAllPaths g [] _ _ = []
findAllPaths g possEdges end dict
    | isNothing (lookup e dict) = error "Bad initialization for the Map."
    | fromJust (lookup e dict) = findAllPaths g xs end dict -- prevent a cycle
    | e == end = [x] -- find the path
    | otherwise = findAllPaths g (xs ++ findEdges g e) end (insert e True dict)
    where x:xs = possEdges
          (s,e,w) = x
