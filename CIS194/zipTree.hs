data Tree a = Node (Tree a) a (Tree a) | Empty

zipTree :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree _ Empty Empty = Just Empty
zipTree _ Empty (Node _ _ _) = Nothing
zipTree _ (Node _ _ _) Empty = Nothing
zipTree f (Node l1 a r1) (Node l2 b r2) =
 case zipTree f l1 l2 of
  Nothing -> Nothing
  Just l  -> case zipTree f r1 r2 of
              Nothing -> Nothing
              Just r -> Just (Node l (f a b) r)

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe ma f = 
 case ma of 
  Nothing -> Nothing
  Just a  -> f a

zipTree' :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree' _ Empty Empty = Just Empty
zipTree' _ Empty (Node _ _ _) = Nothing
zipTree' _ (Node _ _ _) Empty = Nothing
zipTree' f (Node l1 a r1) (Node l2 b r2) =
 bindMaybe (zipTree' f l1 l2) $ \l ->
 bindMaybe (zipTree' f r1 r2) $ \r ->
 Just (Node l (f a b) r)




 
