


xor :: [Bool] -> Bool
xor xs = odd . length . filter (==True) $ xs

