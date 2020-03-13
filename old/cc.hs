

cc xs = (\x -> x `mod` 10 == 0) . addboth . reverse . toDigRev $ xs

addboth xs = (deo xs) + (feo xs) 

deo xs = (\x -> x * 2) . sum $ fmap (xs !!) [1,3..((length xs)-1)]

feo xs = sum $ fmap (xs !!) [0,2..((length xs)-1)]

toDig x
  | x>0 = toDig (x `div` 10) ++ [(x `mod` 10)]
  | otherwise = []

toDigRev x = reverse . toDig $ x 
