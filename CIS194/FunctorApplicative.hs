data ComplicatedA a b
    = Con1 a b
    | Con2 [Maybe (a -> b)]

data ComplicatedB f g a b
    = Con3 (f a)
    | Con4 (g b)
    | Con5 (g (g [b]))


instance Functor (ComplicatedA a) where
 fmap f (Con1 a b) = Con1 a (f b)
 fmap f (Con2 ms)  = Con2 (map (fmap (f .)) ms)

instance Functor g => Functor (ComplicatedB f g a) where
 fmap f (Con3 fa) = (Con3 fa)
 fmap f (Con4 gb) = (Con4 (fmap f gb))
 fmap f (Con5 ggbs) = Con5 (fmap (fmap  (fmap f)) ggbs)

func0 :: Monad f => (a -> a) -> f a -> f a
func0 f xs = do
    x <- xs
    return (f (f x))

func0' :: Functor f => (a -> a) -> f a -> f a
func0' f xs = (f . f) <$> xs

func1 :: Monad f => f a -> f (a,a)
func1 xs = xs >>= (\x -> return (x,x))

func1' :: Functor f => f a -> f (a,a)
func1' xs = fmap (\x -> (x,x)) xs

func2 :: Monad f => f a -> f (a,a)
func2 xs = xs >>= (\x -> xs >>= \y -> return (x,y))

func2' :: Applicative f => f a -> f (a,a)
func2' xs = (,) <$> xs <*> xs

{-
(>>=) :: Monad m => m a -> (a -> m b) -> m b
f <$> ma = fmap f ma
(<*>) :: (a ->
-}

func3 :: Monad f => f a -> f (a,a)
func3 xs = xs >>= (\x -> xs >>= \y -> return (x,x))

func3' :: Applicative f => f a -> f (a,a)
func3' xs = (\x _ -> (x,x)) <$> xs <*> xs

func4 :: Monad f => f a -> f a -> f (a,a)
func4 xs ys = xs >>= (\x -> ys >>= \y -> return (x,y))

func4' :: Applicative f => f a -> f a -> f (a,a)
func4' xs ys = (,) <$> xs <*> ys

func5 :: Monad f => f Integer -> f Integer -> f Integer
func5 xs ys = do
    x <- xs
    let x' = x + 1
    y <- (+1) <$> ys
    return (x' + y)

func5' :: Applicative f => f Integer -> f Integer -> f Integer
func5' xs ys = (+) <$> (+2) <$> xs <*> ys

func5'' :: Applicative f => f Integer -> f Integer -> f Integer
func5'' xs ys = (\x y -> (x + 1) + (y + 1)) <$> xs <*> ys

func6 :: Monad f => f Integer -> f (Integer,Integer)
func6 xs = do
    x <- xs
    return $ if x > 0 then (x, 0)
                      else (0, x)

func6' :: Functor f => f Integer -> f (Integer,Integer)
func6' xs = (\x -> if x > 0 then (x,0) else (0,x)) <$> xs

func6'' :: Functor f => f Integer -> f (Integer,Integer)
func6'' = fmap (\x -> if x > 0 then (x,0) else (0,x))

func7 :: Monad f => f Integer -> f (Integer,Integer)
func7 xs = do
    x <- xs
    if x > 0 then return (x, 0)
             else return (0, x)
-- but not exact, because the first argument is always eval'd in 7, but not in 6. Can't implement true 7 with anything other than Monad. Because fmap returns the functor wrapping always, and x doesn't have to be eval'd, but in 7, x is always eval'd
func7' :: Monad f => f Integer -> f (Integer,Integer)
func7' = func6'

func8 :: Monad f => f Integer -> Integer -> f Integer
func8 xs x = pure (+) <*> xs <*> pure x

func8' :: Functor f => f Integer -> Integer -> f Integer
func8' xs x = (+x) <$> xs

func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

func10 :: Monad f => f Integer -> f Integer
func10 xs = do
    x <- xs >>= (\x -> return (x * x))
    return (x + 10)

func10' :: Functor f => f Integer -> f Integer
func10' = fmap (\x -> (x * x) + 10)







