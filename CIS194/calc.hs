{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Parser
import StackVM
import qualified ExprT as E

eval :: E.ExprT -> Integer
eval exprT = case exprT of
  E.Lit a -> a
  E.Add a b -> (eval a) + (eval b)
  E.Mul a b -> (eval a) * (eval b)

evalStr :: String -> Maybe Integer
evalStr str = case (parseExp E.Lit E.Add E.Mul str) of
  Nothing -> Nothing
  Just e -> Just (eval e)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr E.ExprT where
  lit = E.Lit
  add = E.Add
  mul = E.Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit a = (a > 0)
  add = (||)
  mul = (&&)

instance Expr Program where
  lit a = [PushI a] :: Program
  add a b = b ++ a ++ [Add] :: Program
  mul a b = b ++ a ++ [Mul] :: Program

compile :: String -> Maybe Program
compile str = case (parseExp E.Lit E.Add E.Mul str) of
  Nothing -> Nothing
  Just e -> Just (evalProgram e) 

evalProgram :: E.ExprT -> Program
evalProgram exprT = case exprT of
  E.Lit a -> lit a
  E.Add a b -> add (evalProgram a) (evalProgram b)
  E.Mul a b -> mul (evalProgram a) (evalProgram b)

run :: String -> Maybe (Either String StackVal)
run str = case (compile str) of
  Nothing -> Nothing
  Just p  -> Just (stackVM p)

{-stackVM :: Program -> Either String StackVal-}
