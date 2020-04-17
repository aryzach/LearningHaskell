respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") allLines)  
    where   isPalindrome xs = xs == reverse xs  
            allLines = lines contents

yeah x = unlines [numlines,
                  "Number of empty lines: " ++ numempty,
                  "Number of words: " ++ numwords,
                  "Number of unique words: " ++ unique,
                  "Number of words followed by themselves: " ++ followed,
                  "Length of the longest line: " ++ longest]
  where allLines = lines x
        numlines = "Number of lines: " ++ (show $ length allLines)
        numempty = "0" 
        numwords = "0" 
        unique = "0"
        followed = "0"
        longest = "0"

main = interact yeah 

{-
numlines1 x = length . lines $ x
numempty1 x = foldr (\x y -> if (length x) > 1 then (0+) else (1+)) 0 $ lines x
-}






