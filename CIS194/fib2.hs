

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib (x-2)



fibs2 :: [Integer]
fibs2 = 1 : 1 : mapBar fibs2

mapBar :: [Integer] -> [Integer]
mapBar (x:y:xs) = x + y : mapBar (y:xs) 

data Stream a = Cons a (Stream a)
instance Show a => Show (Stream a) where
 show (Cons a s) = show $ take 20 $ streamToList (Cons a s)


streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : (streamToList s)

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f a = let b = f a in
 Cons a (streamIterate f b)

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons a s) st = Cons a (streamInterleave st s)

nats :: Stream Integer
nats = streamIterate (+1) 1

ruler :: Stream Integer
ruler = h 0 

h :: Integer -> Stream Integer
h a = streamInterleave (streamRepeat a) (h (a+1))














