

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x y -> f x (myFoldl f  


map2 :: (a -> b) -> [a] -> [b]
map2 f = foldr (\x y -> (f x):y) [] 


