module Classthree where

import Data.List

fatorial :: Int -> Int
fatorial n = if n <= 1 
             then 
                1 
             else 
                n * fatorial (n-1)

-- Guards, quantas quiser, precisa ser booleano, o primeiro que bater bateu (não é patter matching)
fatorial' :: Int -> Int
fatorial' n | n <= 1  = 1
            | otherwise =  n * fatorial (n-1)      

-- Pattern matching '_' wildcard
is_zero :: Integer -> Bool
is_zero 0 = True
is_zero _ = False          

-- Accumulators - tail recursive function 

fatorial'' :: Integer -> Integer
fatorial'' n = aux n 1
    where
        aux n acc
         | n <= 1        = acc 
         | otherwise     = aux (n-1) (n*acc)


makeAscList :: Integer -> Integer  -> [Integer]
makeAscList n m | m < n  = []
                | m == n = [m]
                | m > n  = n : makeAscList (n+1) m

-- head
-- tail

--   List Comprehension
-- Classthree> [2*x | x <- [1,2,3], x > 1]
-- [4,6]
-- Classthree> [2*x | x <- [1,2,3]]
-- [2,4,6]
-- Classthree> [ (x, y) | x <- [1,2,3], y <- ['a', 'b']]
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]


addTuples :: [(Integer,Integer)] -> [Integer]
addTuples xs = [ x + y | (x,y) <- xs]


-- Class Five exercises
search :: [Integer] -> Integer -> Bool
search [] _     = False
search (x:xs) e = (e == x) || (search xs e)

removeDuplicates :: [Integer] -> [Integer]
removeDuplicates [] = []
removeDuplicates (x:xs) | search xs x = (removeDuplicates xs)
                        | otherwise   = x : (removeDuplicates xs)

isAsc :: [Integer] -> Bool
isAsc []     = True 
isAsc (x:xs) | (isNill xs)   = True
             | x <= (head xs) = isAsc xs
             | otherwise     = False

isAsc' :: [Integer] -> Bool
isAsc' []       = True
isAsc' [x]      = True 
isAsc' (x:y:xs) = (x <= y) && (isAsc (y:xs))


isNill :: [Integer] -> Bool
isNill [] = True
isNill _  = False 

--[(1,2),(2,3),(3,2),(4,3),(4,5)]
hasPath :: [(Integer,Integer)] -> Integer -> Integer -> Bool
hasPath [] x y = x == y
hasPath xs x y | x == y = True
               | otherwise = 
                let xs' = [(n,m) | (n,m) <- xs, n /= x]
                in or [hasPath xs' m y | (n,m) <- xs, n == x]






