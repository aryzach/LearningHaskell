import Data.Char

main = interact $ (unlines . filter ('a' `elem`) . lines) 
