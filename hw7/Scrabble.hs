module Scrabble where

import Data.Char (toUpper)


newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = 0

score :: Char -> Score
score x
    | y `elem` "AEILNORSTU" = 1
    | y `elem` "DG"         = 2
    | y `elem` "BCMP"       = 3
    | y `elem` "FHVWY"      = 4
    | y `elem` "K"          = 5
    | y `elem` "JX"         = 8
    | y `elem` "QZ"         = 10
    | otherwise             = 0
  where y = toUpper x

scoreString :: String -> Score
scoreString = foldr ((+) . score) 0
