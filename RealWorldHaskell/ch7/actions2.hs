

str2msg :: String -> String
str2msg = (++) "data: "

str2act :: String -> IO String
str2act s = do (putStrLn . str2msg) s
               return $ str2msg s
nums :: [Int]
nums = [1..10]

main = do str2act "start"
          x <- mapM_ (str2act . show) nums
          print x
          str2act "done"







