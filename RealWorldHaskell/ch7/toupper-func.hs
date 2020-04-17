


import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do 
       inh <- openFile "input2.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       inpStr <- hGetContents inh
       let result = process inpStr
       hPutStr outh result
       hClose inh
       hClose outh

process :: String -> String
process = map toUpper

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
    do ineof <- hIsEOF inh
       if ineof
           then return ()
           else do inpStr <- hGetLine inh
                   hPutStrLn outh (map toUpper inpStr)
                   mainloop inh outh




