{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where


-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = fib <$> [0..]


-- Exercise 2
fibs2 :: [Integer]
fibs2 = go 0 1
  where go x y = x : go y (x + y)


-- Exercise 3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList


-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Stream x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x $ streamFromSeed f (f x)


-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) ys = Stream x $ interleaveStreams ys xs

ruler :: Stream Integer
ruler = go 0
  where go x = streamRepeat x `interleaveStreams` go (x + 1)


-- Exercise 6
x :: Stream Integer
x = streamMap (\x -> if x == 1 then 1 else 0) nats

instance Num (Stream Integer) where
    fromInteger x                   = Stream x $ streamRepeat 0
    negate                          = streamMap negate
    (Stream x xs) + (Stream y ys)   = Stream (x + y) (xs + ys)
    (Stream x xs) * b@(Stream y ys) = Stream (x * y) xTerms
      where xTerms = streamMap (*x) ys + xs * b

instance Fractional (Stream Integer) where
    a@(Stream x xs) / b@(Stream y ys) = Stream (x `div` y) xTerms
      where xTerms = streamMap (`div` y) $ xs - ys * (a / b)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)


-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer
  deriving (Show, Eq)

instance Num Matrix where
    (Matrix a1 b1 c1 d1) * (Matrix a2 b2 c2 d2)
      = Matrix (a1*a2 + b1*c2) (a1*b2 + b1*d2) (c1*a2 + d1*c2) (c1*b2 + d1*d2)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = fn
  where Matrix _ fn _ _ = Matrix 1 1 1 0 ^ n