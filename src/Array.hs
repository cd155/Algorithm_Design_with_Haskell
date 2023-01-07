module Array where
import Data.Map (Map, insert, member, adjust, empty)

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

{-
    1.2
    Given two strings, write a method to decide if two string have 
    the set with the other.
-}

isPerm :: String -> String -> Bool
isPerm xs ys = convToDict xs empty == convToDict ys empty

convToDict :: String -> Map Char Int -> Map Char Int
convToDict [] dict = dict
convToDict (x:xs) dict
    | x `member` dict = convToDict xs (adjust (1+) x dict)
    | otherwise = convToDict xs (insert x 1 dict)
