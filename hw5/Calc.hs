{-# LANGUAGE FlexibleInstances #-}

module Calc where

import qualified Data.Map as M

import qualified ExprT
import qualified StackVM
import Parser


-- Exercise 1
eval :: ExprT.ExprT -> Integer
eval expr = case expr of
    ExprT.Lit x   -> x
    ExprT.Add x y -> eval x + eval y
    ExprT.Mul x y -> eval x * eval y


-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul


-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT.ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul


-- Exercise 4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (>0)
    add = (||)
    mul = (&&)

instance Expr MinMax where
    lit                       = MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

instance Expr Mod7 where
    lit                   = Mod7
    add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7


-- Exercise 5
instance Expr StackVM.Program where
    lit x   = [StackVM.PushI x]
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul


-- Exercise 6
class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
    lit = Lit
    add = Add
    mul = Mul

instance HasVars VarExprT where
    var = Var

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit         = const . pure
    add x y map = (+) <$> x map <*> y map
    mul x y map = (*) <$> x map <*> y map

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup