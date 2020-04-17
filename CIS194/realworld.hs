import Data.Char
import Data.List
import Text.Read

halveEvens :: [Integer] -> [Integer]
halveEvens = map (`div` 2) . filter even

safeString :: String -> String
safeString = map (\x -> if (isControl x) then '_' else x) 

getList x = zipWith (\n l -> (n,l)) [1..(length x)] $ take (length x) . repeat $ zipWith (\y z -> (y,z)) [1..(length x)] x 

holes :: [a] -> [[a]]
holes s = map (\f -> map (\(b,c) -> c) f) $ map (\(n , l) -> filter (\(i , c) -> n /= i) l) $ getList s

longestText :: Show a => [a] -> a
longestText (x:xs) = foldr give x xs

textCompare :: Show a => a -> a -> Ordering
textCompare a b = (compare (length $ show a) (length $ show b)) <> GT

give a b = case (textCompare a b) of
 GT -> a
 LT -> b

lengthCompare'' :: String -> String -> Ordering
lengthCompare'' a b = (compare (length a) (length b)) <> (compare (vowels a) (vowels b)) <> (compare a b)
 where vowels = length . filter (`elem` "aeiou")

adjacents :: [a] -> [(a,a)]
adjacents [] = []
adjacents (a:[]) = []
adjacents (a:b:as) = (a,b) : (adjacents (b:as))

commas :: [String] -> String
commas [] = []
commas (a:[]) = a
commas (a:b:bs) = a ++ ", " ++ commas (b:bs)

addPolynomials :: [[Integer]] -> [Integer]
addPolynomials [[]] = []
addPolynomials (a:[]) = a
addPolynomials (a:b:bs) = addPolynomials ((zipWith (+) a b) : bs)

sumNumbers :: String -> Maybe Int
sumNumbers s = fmap sum . sequence $ map (\b -> (readMaybe b :: Maybe Int)) $ final $ numlist s

final [] = [[]]
final (x:[]) = if x == "." then [[]] else [x]
final (x:y:xs) = if y /= "." then final ((x ++ y) : xs) else x : (final xs) 


numlist a = map (\x -> [x]) $ shorter a

shorter [] = []
shorter (a:[]) = case (isDigit a) of
 True -> [a]
 False -> []
shorter (a:b:xs) 
 | ad && bd       = a : (shorter (b:xs))
 | ad && (not bd) = a : '.' : (shorter xs)
 | (not ad) && bd = shorter (b:xs)
 | not (ad || bd) = shorter xs
 where ad = isDigit a
       bd = isDigit b



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
    [ "Enter text: \n\n"   
    , "Number of lines: " ++ (show numLines)
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


main = interact $ wordCount . read
