contains :: String -> [String] -> Bool
contains _ [] = False
contains str (x:xs) = str == x || contains str xs

unique :: [String] -> [String] -> [String]
unique uniqueList list
  | list == [] = uniqueList
  | not (contains x uniqueList) = unique (x : uniqueList) xs
  | otherwise = unique uniqueList xs
  where
    (x:xs) = list

wordCount :: String -> String
wordCount str =
  unlines
    [ "Number of lines: " ++ (show numLines)
    , "Number of emptylines: " ++ (show emptyLines)
    , "Number of words: " ++ (show numWords)
    , "Number of unique words: " ++ (show uniqueWords)
    , "Number of words followed by themselves: " ++ (show sameWords)
    , "Length of the longest line: " ++ (show longestLine)
    ]
  where
    allLines = lines str
    numLines = length allLines
    emptyLines = (length . filter (== "")) allLines
    allWords = words str
    numWords = length allWords
    uniqueWords = length $ unique [] allWords
    sameWords = (length . filter (\(x, y) -> x == y) . adjacents) allWords
    longestLine = longestText allLines
