{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import StackVM
import Parser

class Expr a where
 lit :: Integer -> a
 mul :: a -> a -> a
 add :: a -> a -> a

instance Expr Program where
 lit a = [PushI a]
 mul a b = a ++ b ++ [Mul] 
 add a b = a ++ b ++ [Add]

compile :: String -> Maybe Program
compile = parseExp lit add mul 

runStack :: Maybe Program -> Either String StackVal
runStack Nothing = Left "bad"
runStack (Just a) = (stackVM a)

