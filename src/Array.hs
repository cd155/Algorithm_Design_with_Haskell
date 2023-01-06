module Array where

{-
    1.1
    Implement an algorithm to determine if a string has all unique 
    characters. What if you cannot use additional data structures?
-}
isUniq :: Eq a => [a] -> Bool
isUniq xs = isUniqHelper xs []

isUniqHelper :: Eq a => [a] -> [a] -> Bool
isUniqHelper [] _ = True
isUniqHelper (x:xs) dict
    | x `elem` dict = False
    | otherwise = isUniqHelper xs (x:dict)

-- check unique without structure
isUniqBF :: Eq a => [a] -> Bool
isUniqBF [] = True
isUniqBF (x:xs) = isUniqBFHelper x xs && isUniqBF xs

-- compare an element with a list
isUniqBFHelper :: Eq a => a -> [a] -> Bool
isUniqBFHelper _ [] = True
isUniqBFHelper c (x:xs) = (c /= x) && isUniqBFHelper c xs

{-
    isUniqBF code animation:

    r1:
        abc
        a bc => a /= b && a /= c

    r2:
        bc
        b c => b /= c

    final: r1 && r2
-}
