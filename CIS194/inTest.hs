countLines :: String -> String
countLines x = "allLines: " ++ allLines
  where allLines = show . length . lines $ x
main = interact countLines


