{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- module myModel where

import ExprT ( ExprT(..) )
import Parser ( parseExp )
import StackVM ( Program, StackExp(Mul, PushI, Add) )
-- Exercise 1
eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add expr1 expr2) = eval expr1 + eval expr2
eval (ExprT.Mul expr1 expr2) = eval expr1 * eval expr2

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit :: Integer -> ExprT
  lit = ExprT.Lit
  add :: ExprT -> ExprT -> ExprT
  add = ExprT.Add
  mul :: ExprT -> ExprT -> ExprT
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit :: Integer -> Integer
  lit x = x
  add :: Integer -> Integer -> Integer
  add x y = x + y
  mul :: Integer -> Integer -> Integer
  mul x y = x * y

instance Expr Bool where
  lit :: Integer -> Bool
  lit = (/= 0)
  add :: Bool -> Bool -> Bool
  add = (||)
  mul :: Bool -> Bool -> Bool
  mul = (&&)

instance Expr MinMax where
  lit :: Integer -> MinMax
  lit = MinMax
  add :: MinMax -> MinMax -> MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul :: MinMax -> MinMax -> MinMax
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit :: Integer -> Mod7
  lit = Mod7 . (`mod` 7)
  add :: Mod7 -> Mod7 -> Mod7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul :: Mod7 -> Mod7 -> Mod7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

testExp :: (Expr a) => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

-- Exercise 5
instance Expr StackVM.Program where
  lit :: Integer -> Program
  lit = (: []) . StackVM.PushI
  add :: Program -> Program -> Program
  add x y = x ++ y ++ [StackVM.Add]
  mul :: Program -> Program -> Program
  mul x y = x ++ y ++ [StackVM.Mul]

complie :: String -> Maybe Program
complie = parseExp lit add mul

-- Exercise 6
-- [TODO]