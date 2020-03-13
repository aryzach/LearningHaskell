

module Main where

import System.IO


main = do
  b <- getLine
  a <- readFile b
  putStrLn "write to which file?"
  f <- getLine
  writeFile f (reverse a)
  putStrLn (reverse a)
  
