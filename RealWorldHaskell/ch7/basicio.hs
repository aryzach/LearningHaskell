main = do
       putStrLn "Greetings!  What is your name?"
       inpStr <- getLine
       let y = "ok"
       let y = thing inpStr
       putStrLn $ y


thing :: String -> String
thing s = "yeah " ++ s ++ " ok"


