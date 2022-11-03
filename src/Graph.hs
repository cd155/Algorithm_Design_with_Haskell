module Graph where

import Basics (Nat)

-- representing Graph
type Graph = ([Vertex], [Edge])
type Edge = (Vertex, Vertex, Weight)
type Vertex = Nat -- represent by node one, node two...
type Weight = Int

