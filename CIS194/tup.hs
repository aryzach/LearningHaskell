import Control.Monad




main = readLn >>= \n -> if n > 5 
 then putStrLn (show (n + 1)) 
 else putStrLn "no"


data Tree a = Node (Tree a) a (Tree a) | Empty deriving (Show)

zipTree :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree _ Empty Empty = Just Empty
zipTree _ Empty _     = Nothing
zipTree _ _ Empty     = Nothing
zipTree f (Node l1 a r1) (Node l2 b r2) =
 zipTree f l1 l2 >>= \ml -> 
  zipTree f r1 r2 >>= \mr ->
   Just (Node ml (f a b) mr)






