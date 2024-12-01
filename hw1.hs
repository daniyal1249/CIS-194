module Main where

main :: IO ()
main = putStrLn "Hello World"

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = [read [num] | num <- show n]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = [if even i then 2*num else num | (num, i) <- zip xs [len, len-1..1]]
    where len = length xs

sumDigits :: [Integer] -> Integer
sumDigits xs = sum [sum $ toDigits n | n <- xs]

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther $ toDigits n) `mod` 10 == 0