module Golf where

import Data.List
import Data.Maybe

{-histogram :: [Integer] -> String -}
histogram xs = unlines $ (theList xs) ++ ["0123456789"]


theList xs = filter (any ('*'==)) $ map ifAst $ reverse $ freqsLeft $ freqlist xs

redu :: (Num b) => b -> [(a,b)] -> [(a,b)]
redu a xs = map (\(x,y) -> (x,(y-a))) xs

{-freqsLeft :: (Num a, Enum a, Ord a) => [(b,a)] -> [[(b,a)]]-}
{-freqsLeft xs = (map redu [0..((maxOfFreqList xs)-1)]) <*> (map (\y -> xs) [1..(maxOfFreqList xs)])-}
freqsLeft xs = doForEach (map redu [0..((maxOfFreqList xs)-1)]) (map (\y -> xs) [1..(maxOfFreqList xs)]) 

doForEach (f:fs) (e:es) = (f e) : (doForEach fs es)
doForEach [] _ = []

maxOfFreqList :: Ord a => [(b,a)] -> a
maxOfFreqList xs = maximum $ map snd xs

ifAst :: (Num a,Ord a) => [(a,a)] -> String
ifAst xs = concat $ map (\(x,y) -> if y > 0 then "*" else " ") xs

{-freqlist :: (Integer a) => [a] -> [(a,a)]-}
freqlist xs =  map c $ zip [0..9] $ map (\y -> xs) [0..9]


c :: (Int, [Int]) -> (Int, Int)
c (a,xs) = (a, length (filter (\x -> x == a) xs))

skips :: [a] -> [[a]]
skips [] = []
skips xs = zipWith getOnlyModZfromListA [1..] (lalas xs)

{-getListOfFuns :: [a] -> [[a]]
getListOfFuns = (map getOnlyModZfromListA [1..])-}


lalas :: [a] -> [[a]]
lalas x = map (\y -> x) [1..(length x)]

getOnlyModZfromListA :: Int -> [a] -> [a]
getOnlyModZfromListA i s = map (\(x,y) -> y) (filter (\(x,y) -> if x == i then True else False) (zip (cycle [1..i]) s)) 

lm :: [Int] -> [Int]
lm xs = map getMid $ filter locMax (ct xs)  

ct :: [a] -> [(a,a,a)]
ct (x:y:z:a) = (x,y,z) : (ct (y:z:a))
ct _ = []

getMid :: (a,a,a) -> a
getMid (a,b,c) = b

locMax :: (Ord a, Num a) => (a,a,a) -> Bool
locMax (x,y,z) = y > x && y > z
locMax _ = False
