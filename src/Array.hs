module Array where
import Data.Map (Map, insert, member, adjust, empty)
import Data.Char (ord)

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

-- dictionary comparison
isPerm :: String -> String -> Bool
isPerm xs ys = convToDict xs empty == convToDict ys empty

convToDict :: String -> Map Char Integer -> Map Char Integer
convToDict [] dict = dict
convToDict (x:xs) dict
    | x `member` dict = convToDict xs (adjust (1+) x dict)
    | otherwise = convToDict xs (insert x 1 dict)

-- ASCII array comparison
-- initialize 128 ASCII array
acciiArr = replicate 128 0

isPerm' :: String -> String -> Bool
isPerm' xs ys = fillAccii xs acciiArr == fillAccii ys acciiArr

fillAccii :: String -> [Integer] -> [Integer]
fillAccii [] arr = arr
fillAccii (x:xs) arr = 
    fillAccii xs (init firstHalf ++ [updated] ++ secondHalf)
    where (firstHalf, secondHalf) = splitAt (ord x) arr
          updated = (arr !! (ord x - 1)) + 1

{-
    1.3
    Write a method to replace all spaces in a string with '%20'. 
    You may assume that the string has sufficient space at the 
    end to hold the additional characters,and that you are given 
    the "true" length of the string.
-}
-- replace spaces with 20%, need to remove head(20%) and tail(20%)
repSpace :: String -> String
repSpace xs = repSpaceHelper xs False

repSpaceHelper :: String -> Bool -> String
repSpaceHelper [] _ = []
repSpaceHelper (x:xs) isSpaPre
    | x == ' ' && (not isSpaPre) = "20%" ++ repSpaceHelper xs (not isSpaPre)
    | x == ' ' && isSpaPre = repSpaceHelper xs isSpaPre
    | otherwise = x:repSpaceHelper xs False

