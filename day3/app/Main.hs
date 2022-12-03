module Main where

import System.Environment
import qualified MyLib (prioSum)


main :: IO ()
main = getArgs >>= parse 

parse ["--part1", path] = do
  input <- readFile path
  putStrLn $ show $ MyLib.prioSum input


{-
parse ["--part2", path] = do
  input <- readFile path
  putStrLn $ show $ MyLib.getScorePart2 input
-}