module Main where

import System.Environment
import qualified MyLib (someFunc, getScore)


main :: IO ()
main = getArgs >>= parse 

parse [path] = do
  input <- readFile path
  putStrLn $ show $ MyLib.getScore input

