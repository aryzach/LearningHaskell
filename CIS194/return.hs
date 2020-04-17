data Tree a = Empty | Node (Tree a) a (Tree a)

instance Show a => Show (Tree a) where
 show = showsTree

showsTree               :: (Show a) => Tree a -> String
showsTree Empty    =  show "no"
showsTree (Node l x r) =  showsTree l  ++ showsTree r



mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (a:as) = f a >>= \b -> mapM' f as >>= \bs -> return (b:bs)
{-

(>>) :: Monad m => m a -> m b -> m b

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c

(>>=) :: Monad m => m a -> (a -> m b) -> m b

return :: Monad m => a -> m a

class Monad' a where
 return a >>= k = k a
 m >>= return = m
 m >>= (\x -> k x >>= h) = (m >>= k) >>= h 
-}

zipTree :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree _ Empty Empty        = Just Empty
zipTree _ Empty _ = Nothing
zipTree _ _ Empty = Nothing
zipTree f (Node l1 x r1) (Node l2 y r2) =
 do
  l <- zipTree f l1 l2
  r <- zipTree f r1 r2
  return (Node l (f x y) r)


















