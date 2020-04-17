import Prelude hiding (foldr, foldl)

foldr :: (a -> a -> a) -> a -> [a] -> a 
foldr f x []     = x
foldr f x (y:ys) = f y $ foldr f x ys

foldl :: (a -> a -> a) -> a -> [a] -> a 
foldl f x []     = x
foldl f x (y:ys) = foldl f (f x y) ys

foldl' :: (a -> a -> a) -> a -> [a] -> a 
foldl' f x []     = x
foldl' f x (y:ys) = let g = f x y
                    in seq g $ foldl' f g ys


