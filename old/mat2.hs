import Data.List

diagonalDifference arr = do
    -- Write your code here
    let l = round . sqrt . fromIntegral . Data.List.length $ arr
        z = Data.List.zip [0..] arr
        first = sum . Data.List.map snd . Data.List.filter (\(x,y) -> x `mod` (l + 1) == 0) $ z
        second = sum . Data.List.map snd . Data.List.filter (\(x,y) -> x `mod` 2 == 0) . Data.List.init . Data.List.tail . Data.List.tail $ z
    abs (first - second)
