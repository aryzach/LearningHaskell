--https://blog.tmorris.net/posts/20-intermediate-haskell-exercises/


class Fluffy f where
  furry :: (a -> b) -> f a -> f b

--Exercise 1
-- -- Relative Difficulty: 1
instance Fluffy [] where
 furry = map 
--
--   -- Exercise 2
--   -- Relative Difficulty: 1
instance Fluffy Maybe where
 furry = fmap
--
--     -- Exercise 3
--     -- Relative Difficulty: 5
--     instance Fluffy ((->) t) where
--       furry = error "todo"
--
newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)
--
--       -- Exercise 4
--       -- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
 furry f fa = case fa of
  EitherLeft (Left a)  -> EitherLeft (Left $ f a) 
  EitherLeft (Right b) -> EitherLeft (Right b)
--
--         -- Exercise 5
--         -- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
 furry f fa = case fa of
  EitherRight (Left a)  -> EitherRight (Left a)
  EitherRight (Right b) -> EitherRight (Right $ f b) 
--
class Misty m where
 banana :: (a -> m b) -> m a -> m b
 unicorn :: a -> m a
 furry' :: (a -> b) -> m a -> m b
 furry' f ma = banana (\a -> unicorn $ f a) ma
--
--                         -- Exercise 7
--                         -- Relative Difficulty: 2
instance Misty [] where
 banana f [] = []
 banana f (x:xs) = f x ++ banana f xs 
 unicorn x = [x] 
--
--                             -- Exercise 8
--                             -- Relative Difficulty: 2
instance Misty Maybe where
 banana _ Nothing = Nothing
 banana f (Just x) = f x  
 unicorn = Just
--
--                                 -- Exercise 9
--                                 -- Relative Difficulty: 6
--                                 instance Misty ((->) t) where
--                                   banana = error "todo"
--                                     unicorn = error "todo"
--
--                                     -- Exercise 10
--                                     -- Relative Difficulty: 6
instance Misty (EitherLeft t) where
 banana f (EitherLeft (Left a))  = f a
 unicorn = EitherLeft . Left
--
--newtype EitherLeft b a = EitherLeft (Either a b)
--newtype EitherRight a b = EitherRight (Either a b)
--                                         -- Exercise 11
--                                         -- Relative Difficulty: 6
instance Misty (EitherRight t) where
 banana f (EitherRight (Right a)) = f a  
 unicorn = EitherRight . Right 
--
--                                             -- Exercise 12
--                                             -- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean m = banana (\x -> x) m
--
--                                             -- Exercise 13
--                                             -- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple ma mf = banana (\f -> furry' f ma) mf

--banana :: (a -> m b) -> m a -> m b

--
--                                             -- Exercise 14
--                                             -- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy [] _ = unicorn []
moppy xs f = sequenceA' $ map f xs

sequenceA' :: (Misty m) => [m a] -> m [a]
sequenceA' [] = unicorn []  
sequenceA' (x:xs) = apple (sequenceA' xs) $ (furry' (:) x)
--
--                                             -- Exercise 15
--                                             -- Relative Difficulty: 6
--                                             -- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage xs = moppy xs (\x -> x)
--
--                                             -- Exercise 16
--                                             -- Relative Difficulty: 6
--                                             -- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f ma mb = apple mb (furry' f ma) 
--
--                                             -- Exercise 17
--                                             -- Relative Difficulty: 6
--                                             -- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f ma mb mc = apple mc (banana2 f ma mb)
--
--                                             -- Exercise 18
--                                             -- Relative Difficulty: 6
--                                             -- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f ma mb mc md = apple md (banana3 f ma mb mc)
--
newtype State s a = State {
 state :: (s -> (s, a))
 }
--
--                                               -- Exercise 19
--                                               -- Relative Difficulty: 9
--instance Fluffy (State s) where
-- furry f (State s q) = State
--   { (\z -> let (s1, a1) = q z
--              in (s1, f a1)) }
--
--                                                 -- Exercise 20
--                                                 -- Relative Difficulty: 10
--                                                 instance Misty (State s) where
--                                                   banana = error "todo"
--                                                     unicorn = error "todo"
