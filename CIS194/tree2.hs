

data Tree a = Empty
 | Node (Tree a) a (Tree a)
 deriving (Show, Eq)

treeSize :: Tree a -> Integer
treeSize Empty = 0
treeSize (Node l x r) = 1 + treeSize l + treeSize r

treeSum :: Tree Integer -> Integer
treeSum Empty = 0
treeSum (Node l x r) = x + treeSum l + treeSum r

treeDepth :: Tree a -> Integer
treeDepth Empty = 0
treeDepth (Node l x r) = 1 + max (treeDepth l) (treeDepth r)

flatten :: Tree a -> [a]
flatten Empty = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

foldTree ::b -> (b -> a -> b -> b) -> Tree a -> b
foldTree e f Empty = e
foldTree e f (Node l x r) = f (foldTree e f l) x (foldTree e f r)

treeSize' :: Tree a -> Integer
treeSize' = foldTree 0 (\l _ r -> 1 + l + r) 

treeSum' :: Tree Integer -> Integer
treeSum' = foldTree 0 (\l x r -> l + x + r)

treeDepth' :: Tree a -> Integer
treeDepth' = foldTree 0 (\l _ r -> 1 + max l r)

flatten' :: Tree a -> [a]
flatten' = foldTree [] (\l x r -> l ++ [x] ++ r)

treeMax :: (Ord a, Bounded a) => Tree a -> a
treeMax = foldTree minBound (\l x r -> max (max l r) x)

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT

eval :: ExprT -> Integer
eval (Lit i)     = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
exprTFold fl fa fm (Lit i) = fl i
exprTFold fl fa fm (Add e1 e2) = fa (exprTFold fl fa fm e1) (exprTFold fl fa fm e2) 
exprTFold fl fa fm (Mul e1 e2) = fm (exprTFold fl fa fm e1) (exprTFold fl fa fm e2)

eval2 :: ExprT -> Integer
eval2 = exprTFold id (+) (*)

numLiterals :: ExprT -> Integer
numLiterals = exprTFold (\_ -> 1) (+) (+)

numExpressions = exprTFold (\_ -> 1) (\lm lr -> lm + lr + 1) (\lm lr -> lm + lr + 1)

















 
