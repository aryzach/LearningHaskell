


main = do
  putStrLn "quit? y/n"
  ans <- getLine
  if (ans == "y") then 
    return ()
  else do 
    putStrLn "not quitting"
    main
