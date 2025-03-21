module Golf where

import Data.List (transpose, unlines)


-- Exercise 1
skips :: [a] -> [[a]]
skips xs = [[x | (x, y) <- zip xs ys, y `mod` i == 0] | i <- ys]
  where ys = [1..length xs]


-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x1:x2:x3:xs)
    | x2 > x1 && x2 > x3 = x2 : localMaxima (x3:xs)
    | otherwise          = localMaxima (x2:x3:xs)
localMaxima _            = []


-- Exercise 3
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

histogram :: [Integer] -> String
histogram xs = unlines $ reverse $ transpose histList'
  where
    histList  = [show i ++ "=" ++ replicate (count i xs) '*' | i <- [0..9]]
    maxLength = maximum $ map length histList
    histList' = [i ++ replicate (maxLength - length i) ' ' | i <- histList]