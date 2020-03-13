module ExprT where


data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)

data MinMax = MinMax Integer
  deriving (Eq, Ord, Show)

data Mod7 = Mod7 Integer
  deriving (Eq, Ord, Show)



