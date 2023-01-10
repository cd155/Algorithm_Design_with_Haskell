module StackQueue where

newtype Stack a = Stack [a] deriving Show

emptyS = Stack []

pop :: Stack a -> Maybe (Stack a, a)
pop (Stack []) = Nothing
pop (Stack (x:xs)) = Just (Stack xs, x)

push :: a -> Stack a -> Stack a
push x (Stack l) = Stack (x:l)

newtype Queue a = Queue [a] deriving Show

emptyQ = Queue []

deQue :: Queue a -> Maybe (Queue a, a)
deQue (Queue []) = Nothing
deQue (Queue xs) = Just (Queue (init xs), last xs)

enQue :: a -> Queue a -> Queue a
enQue x (Queue l) = Queue (l++[x])

-- alternative the list can be use as Stack or Queue
