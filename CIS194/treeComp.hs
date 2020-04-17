data Tree a = Node (Tree a) a (Tree a) | Empty deriving (Show)



zipTree :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree _ Empty Empty = Just Empty
zipTree _ Empty _ = Nothing
zipTree _ _ Empty = Nothing
zipTree f (Node l1 a r1) (Node l2 b r2) =
 case zipTree f l1 l2 of
  Nothing -> Nothing
  Just left -> case zipTree f r1 r2 of
                              Nothing -> Nothing
                              Just right -> Just (Node left (f a b) right)

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _  = Nothing
bindMaybe (Just a) f = f a

zipTree' :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree' _ Empty Empty = Just Empty
zipTree' _ Empty _ = Nothing
zipTree' _ _ Empty = Nothing
zipTree' f (Node l1 a r1) (Node l2 b r2) =
 bindMaybe (zipTree' f l1 l2) $ \left -> 
  bindMaybe (zipTree' f r1 r2) $ \right ->
   Just (Node left (f a b) right) 
   
zipTree'' :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree'' _ Empty Empty = Just Empty
zipTree'' _ Empty _ = Nothing
zipTree'' _ _ Empty = Nothing
zipTree'' f (Node l1 a r1) (Node l2 b r2) =
 (zipTree'' f l1 l2) >>= \left ->
  (zipTree'' f r1 r2) >>= \right ->
   Just (Node left (f a b) right)

ordPairs :: Ord a => [a] -> [(a, a)]
ordPairs xs =
    xs >>= \x1 ->
    xs >>= \x2 ->
    if x1 < x2 then [(x1,x2)] else []

nats :: [Integer]
nats = 1 : map (+1) nats



