module JoinList where

import Buffer
import Editor
import Scrabble
import Sized


data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


-- Exercise 1
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a <> tag b) a b


-- Exercise 2
getJlSize :: (Sized b, Monoid b) => JoinList b a -> Int
getJlSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _              | i < 0           = Nothing
indexJ 0 (Single _ a)                     = Just a
indexJ i (Append _ a _) | i < getJlSize a = indexJ i a
indexJ i (Append _ a b)                   = indexJ (i - getJlSize a) b
indexJ _ _                                = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty                             = Empty
dropJ n a@(Single _ _)                    = if n < 1 then a else Empty
dropJ n (Append _ a b) | n <= getJlSize a = dropJ n a +++ b
dropJ n (Append _ a b)                    = dropJ (n - getJlSize a) b

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty                             = Empty
takeJ n a@(Single _ _)                    = if n > 0 then a else Empty
takeJ n (Append _ a _) | n <= getJlSize a = takeJ n a
takeJ n (Append _ a b)                    = a +++ takeJ (n - getJlSize a) b


-- Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x


-- Exercise 4
instance Buffer (JoinList (Score, Size) String) where
  toString    = undefined
  fromString  = undefined
  line        = undefined
  replaceLine = undefined
  numLines    = undefined
  value       = undefined

main :: IO()
main = runEditor editor (fromString "Test" :: JoinList (Score, Size) String)
