module Main where

main :: IO ()
main = putStrLn "Hello World"

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = [read [num] | num <- show n]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = [if even i then 2*num else num | (num, i) <- zip xs [len, len-1..1]]
    where len = length xs

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits xs = sum [sum $ toDigits n | n <- xs]

-- Exercise 4
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther $ toDigits n) `mod` 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n <= 0 = []
    | otherwise = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a