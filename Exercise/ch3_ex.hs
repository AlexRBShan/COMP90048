-- Exercises from chapter 3 in real world haskell
--
import Data.List

-- Exercise 1&2
-- Write a function that computes the number of elements in a list.
mylen :: [a] -> Int
mylen [] = 0
mylen (x:xs) = 1 + mylen xs

-- Exercise 3
-- Write a function that computes the mean of a list
getmean :: Fractional p => [p] -> p
getmean [] = 0
getmean x = (sum x) / (fromIntegral (length x))

-- Exercise 4
-- Turn a list into a palindrome;
palindrome :: [a] -> [a]
palindrome [] = []
palindrome x = x ++ reverse x

-- Exercise 5
-- Write a function that determines whether its input list is a palindrome
ifpalindrome :: Eq a => [a] -> Bool
ifpalindrome [] = True
ifpalindrome [a] = False
ifpalindrome (x:xs) = if x /= last xs then False
                      else ifpalindrome (init xs)

-- Exercise 6
-- Create a function that sorts a list of lists based on the length of each sublist
sortlist :: [[a]] -> [[a]]
--sortlist [] = []
--sortlist [a] = [a]
sortlist x  = sortBy lencompare x
    where
        lencompare a b
            | length a < length b = LT
            | length a > length b = GT
            | otherwise = EQ

--Exercise 7&8
--Define a function that joins a list of lists together using a separator value
myintersperse :: a -> [[a]] -> [a]
myintersperse a [] = []
myintersperse a (x:xs)
    | null xs = x
    | otherwise = x ++ [a] ++ myintersperse a xs

-- Exercise 9
-- Find the height of a binary tree type
data Tree a = Empty | Node a (Tree a) (Tree a)
height :: Tree a -> Int
height Empty = 0
height (Node _ l r) = 1 + height l + height r


--Exercise 10




