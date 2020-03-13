import Data.List

plusMinus arr = do
    let l = fromIntegral . length $ arr
        p = (\x -> x/l) . fromIntegral . length . Data.List.filter (\x -> x > 0) $ arr
        n = (\x -> x/l) . fromIntegral . length . Data.List.filter (\x -> x < 0) $ arr
        z = (\x -> x/l) . fromIntegral . length . Data.List.filter (\x -> x == 0) $ arr
    print p
    print n
    print z
