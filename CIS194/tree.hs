data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
 deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree xs = foldr treeFold Leaf xs

treeFold :: a -> Tree a -> Tree a
treeFold x Leaf = Node 0 Leaf x Leaf
treeFold x (Node y Leaf z Leaf) = Node 1 (treeFold x Leaf) z Leaf
treeFold x (Node n Leaf t (Node sn ta oa tb)) = Node n (Node 0 Leaf x Leaf) t (Node sn ta oa tb)
treeFold x (Node n (Node sn ta oa tb) t Leaf) = Node n (Node sn ta oa tb) t (Node 0 Leaf x Leaf)
treeFold x (Node n (Node a b c d) oa (Node e f g h)) 
  | (countLeaf (Node a b c d))==(countLeaf (Node e f g h)) = Node (e+1) (Node a b c d) oa (treeFold x (Node e f g h))
  | (countLeaf (Node a b c d))<(countLeaf (Node e f g h))  = Node n (treeFold x (Node a b c d)) oa (Node e f g h)
  | otherwise = Node n (Node a b c d) oa (treeFold x (Node e f g h))


countLeaf :: Tree a -> Int
countLeaf Leaf = 1
countLeaf (Node n ta x tb) = countLeaf ta + countLeaf tb



