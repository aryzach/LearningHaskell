commas :: [String] -> String
commas [] = []
commas (a:[]) = a
commas (a:b:bs) = a ++ ", " ++ commas (b:bs)


testResults :: [(String, [Bool])]
testResults = [ ("commas",      ex_commas)
              , ("safeString",      ex_commas)
              ]


ex_commas = [ commas [] == ""
   , commas ["Hello"] == "Hello"
   , commas ["Hello", "World"] == "Hello, World"
   , commas ["Hello", "", "Wold"] == "Hello, , World"
   , commas ["Hello", "n0w", "World"] == "Hello, new, World"
   ]


formatTests :: [(String, [Bool])] -> String
formatTests = unlines . map getResults 

getResults :: (String, [Bool]) -> String
getResults (funName, resultList) = funName ++ ": " ++ (testDescription resultList)

testDescription :: [Bool] -> String
testDescription boolList 
 | allTrue = "All tests pass"
 | allFalse = "All tests fail"
 | True        = (show numPass) ++ "/" ++ (show $ length boolList) ++ " pass." ++ whichPass ++ " are good."
 where allTrue = all (==True) boolList
       allFalse = all (==False) boolList
       numPass = foldr (\x -> if (x == True) then (1+) else (0+)) 0 boolList
       whichPass = whichPassFun 1 "" boolList

whichPassFun :: Int -> String -> [Bool] -> String
whichPassFun n s [] = s
whichPassFun n s (x:xs) = if x == True 
 then whichPassFun (n + 1) (s ++ ", " ++ (show n)) xs
 else whichPassFun (n + 1) s xs 



main = putStr $ formatTests testResults
