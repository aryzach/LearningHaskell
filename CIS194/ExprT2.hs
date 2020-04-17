{-# LANGUAGE FlexibleInstances #-}
module ExprT2 where

import Parser
import qualified Data.Map as M


data ExprT = Lit Integer
 | Add ExprT ExprT
 | Mul ExprT ExprT
 deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Mul e1 e2) = (eval e1) * (eval e2)
eval (Add e1 e2) = (eval e1) + (eval e2)

evalStr :: String -> Maybe Integer
evalStr st = case (parseExp Lit Add Mul st) of 
 Nothing -> Nothing
 Just x -> Just (eval x) 

class Expr a where
 lit :: Integer -> a
 mul :: a -> a -> a
 add :: a -> a -> a

instance Expr ExprT where
 lit = Lit 
 mul = Mul
 add = Add

idExprT :: ExprT -> ExprT
idExprT = id

instance Expr Integer where
 lit = id
 mul = (*)
 add = (+)

instance Expr Bool where
 lit a = (a > 0)
 mul = (&&)
 add = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
 lit a = MinMax a
 mul (MinMax a) (MinMax b) = MinMax (min a b)
 add (MinMax a) (MinMax b) = MinMax (max a b)

instance Expr Mod7 where
 lit a = Mod7 (a `mod` 7)
 mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)
 add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

class HasVars a where
 var :: String -> a

data VarExprT = VLit Integer
 | VMul VarExprT VarExprT
 | VAdd VarExprT VarExprT
 | Var String
 deriving (Show, Eq)

instance Expr VarExprT where
 lit = VLit
 mul = VMul
 add = VAdd

instance HasVars VarExprT where
 var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
 var = M.lookup 

instance Expr (M.Map String Integer -> Maybe Integer) where
 lit x = \f -> Just x
 


