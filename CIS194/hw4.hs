


fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (*) 1 . map (\x -> x-2) . filter even

same1 xs = fun1 xs == fun1' xs


fun2 1 = []
fun2 n 
  | even n = n : fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

{-same2 x = fun2 x == fun2' x-}

fun2' n = sum . filter even . takeWhile (/=1) . iterate other $ n

other 1 = 0
other x
  | even x = x `div` 2
  | otherwise = 3 * x + 1












