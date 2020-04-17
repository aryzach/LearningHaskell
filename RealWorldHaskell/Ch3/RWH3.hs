import Data.List

f3 :: (Fractional a, Foldable t) => t a -> a
f3 xs = (\sum -> sum / (fromIntegral $ length xs)) $ foldr (\x y -> x + y) 0 xs

f5 :: Eq a => [a] -> Bool
f5 xs = xs == reverse xs

f6 :: Foldable a =>  [a b] -> [a b]
f6 xs = sortBy (\x y -> compare (length x) (length y)) xs

f7 :: a -> [[a]] -> [a]
f7 x [] = tail [x]
f7 _ (a:[]) = a
f7 x (a:as) = a ++ [x] ++ f7 x as

data Tree a = Empty | Node a (Tree a) (Tree a)

f8 :: Tree a -> Int
f8 Empty = 0
f8 (Node x l r) = 1 + max (f8 l) (f8 r)

data Direction = ALeft | ARight | AStraight deriving (Show)
data Point = P Int Int deriving (Eq, Ord, Show)
data Line = L Point Point

line :: Point -> Point -> Line
line x y = L x y

dir :: Line -> Line -> Maybe Direction
dir (L (P x1 y1) (P x2 y2)) (L (P a b) (P x3 y3)) = 
 if (x2 /= a && y2 /= b) then Nothing
 else Just f
  where f = let cross = ((x2 - x1)*(y3 - y1)) - ((y2-y1)*(x3-x1))
            in if cross == 0 then AStraight
               else if cross > 0 then ALeft
               else ARight

angleFromXAxis :: Point -> Hmm Float
angleFromXAxis (P x y) = case x of
 0 -> Nada
 _ -> Yeah $ (fromIntegral y) / (fromIntegral x) 

f11 :: [Point] -> [Maybe Direction]
f11 (x:y:z:rest) = dir (L x y) (L y z) : f11 (y:z:rest)
f11 _ = []

data Hmm a = Nada | Yeah a deriving (Eq, Show)

instance (Ord a) => Ord (Hmm a) where
 compare Nada Nada = EQ
 compare Nada (Yeah _) = GT
 compare (Yeah _) Nada = LT
 compare (Yeah a) (Yeah b) = compare a b 

grahamScan :: [Point] -> [Point]
grahamScan ps = let p = start ps in helper [p] (sortBy (\a b -> compare (angleFromXAxis a) (angleFromXAxis b))  $ filter (\x -> x /= p) ps)
 where helper good [] = good
       helper (g:[]) (p:ps) = helper (p:g:[]) ps
       helper (a:b:as) (p:ps) = case (dir (line b a) (line a p)) of
        Just ALeft -> helper (p:a:b:as) ps
        Just AStraight -> helper (p:a:b:as) ps
        Just ARight -> helper (b:as) (p:ps)


start :: [Point] -> Point
start (p:ps) = helper p ps
 where helper p [] = p
       helper (P x y) ((P a b):ps) = 
        if y < b || (y == b && x < a)
        then helper (P x y) ps
        else helper (P a b) ps 

p1 = P 1 1
p2 = P 2 5
p3 = P 3 3
p4 = P 4 5
p5 = P 5 0
p6 = P 6 1
p7 = P 0 10

ps = [p1,p2,p3,p4,p5,p6,p7]

