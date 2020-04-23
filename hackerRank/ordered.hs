import Control.Monad


main :: IO()
main = do
 q <- readLn :: IO Int
 forM_ [1..q] $ \q_itr -> do
  s <- getLine
  (print (answer s 0))


answer :: String -> Int -> Int
answer s n = 
    if ordered s then n
    else answer (run s) (n + 1)

run :: String -> String
run [] = []
run as = 
 let (orderedFront, unorderedBack) = mySplit ([],as)
  in myRotate unorderedBack

mySplit :: (String, String) -> (String, String)
mySplit (ord, []) = (ord, [])
mySplit (ord,(un:unord)) = 
 let temp = ord ++ [un]
  in if ordered temp && allSmaller temp unord then mySplit (temp,unord)
     else (ord,(un:unord))

allSmaller :: String -> String -> Bool
allSmaller [] _ = True
allSmaller (a:as) unord = (foldr (\x -> (&&) (a <= x)) True unord) && allSmaller as unord

myRotate :: String -> String 
myRotate [] = []
myRotate s = findBestCycle s 

findBestCycle :: String -> String
findBestCycle s = foldr (\x y -> if x < y then x else y) s (getAllCycles s) 

getAllCycles :: String -> [String]
getAllCycles s = helper (cycleOne s) s []
 where 
  helper :: String -> String -> [String] -> [String]
  helper cur orig soFar =
   if cur == orig then (cycleOne cur : soFar)
   else helper (cycleOne cur) orig (cur : soFar)

ordered :: String -> Bool
ordered [] = True
ordered (a:[]) = True
ordered (a:b:bs) = a <= b && ordered (b:bs)

cycleOne :: String -> String
cycleOne [] = []
cycleOne (a:as) = as ++ [a]



-- I don't end up using these
getAllSubStrings :: String -> [String]
getAllSubStrings [] = [[]]
getAllSubStrings [a] = [[a]]
getAllSubStrings s = helper (cycleOne s) s [] 
 where 
  helper current orig soFar =
   if current == orig then (getSubsForOne current ++ soFar) 
   else helper (cycleOne current) orig (getSubsForOne current ++ soFar)  
getSubsForOne :: String -> [String]
getSubsForOne s = zipWith id (map take [1..(length s)]) (repeat s)  








