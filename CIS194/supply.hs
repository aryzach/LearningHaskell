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



data Supply s a = S (Stream s -> (a, Stream s))

{-
idiom1 :: Supply s a
idiom1 = S (\xs -> …)

idiom2 :: Supply s a
idiom2 = S go
  where go xs = …
-}
get :: Supply s s
get = S (\(Cons a as) -> (a, as))

pureSupply :: a -> Supply s a
pureSupply a = S (\x -> (a, x))

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply f (S g) = S go 
 where go x = let (a, as) = g x
              in (f a, as)

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f (S fa) (S fb) = S fc
 where fc x = let (a , as) = fa x
                  (b , bs) = fb as
              in (f a b, bs)

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S fa) f = S fb 
 where fb x = let (a, as) = fa x
                  (S n)   = f a
              in n as

runSupply :: Stream s -> Supply s a -> a
runSupply s (S f) = let (a, as) = f s
                    in a



data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show
































