{-# LANGUAGE FlexibleInstances #-}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2))

fibs1 :: [Integer]
fibs1 = map fib [1..]

fibs2 :: [Integer]
fibs2 = let fibs2 a b = (a + b) : fibs2 b (a + b) in 0 : 1 : fibs2 0 1

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

instance Show a => Show (Stream a) where
 show = show . take 30 . streamToList  

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) (streamMap f b) 

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a a1) b = Cons a (interleaveStreams b a1)

ruler :: Stream Integer
ruler = rulerHelper 0

rulerHelper :: Integer -> Stream Integer
rulerHelper a = interleaveStreams (streamRepeat a) (rulerHelper (a + 1))

x :: Stream Integer
{-
instance Num (Stream Integer) where
 fromInteger a = Cons a (streamRepeat 0)
 negate = map (* (-1)) 
 (Cons a a1) + (Cons b b2) = Cons (a + b) (a1 + b1)
 (Cons a a1) * (Cons b b2) = Cons (a * b) (a * b2)
-}
