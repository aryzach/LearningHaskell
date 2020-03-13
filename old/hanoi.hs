

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x a b c
  | x == 0 = []
  | x == 1 = (a, c) : hanoi 0 a b c 
  | otherwise = hanoi (x - 1) a c b ++ ((a, c) : hanoi (x - 1) b a c)


hanoi2 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi2 x a b c d
  | x == 0 = []
  | x == 1 = (a, d) : []
  | x == 2 = hanoi x a b d
  | otherwise = hanoi2 (x - 2) a b d c ++ ((a, b) : (a, d) : (b, d) : hanoi2 (x - 2) c a b d)
