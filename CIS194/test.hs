main :: IO ()
main = putStrLn "enter number: " >>
       readLn >>= \n ->
       putStrLn (show $ n + 1)













