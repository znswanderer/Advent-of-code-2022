module Main where

import System.Environment
import qualified MyLib (getScore, getScorePart2)


main :: IO ()
main = getArgs >>= parse 

parse ["--part1", path] = do
  input <- readFile path
  putStrLn $ show $ MyLib.getScore input


parse ["--part2", path] = do
  input <- readFile path
  putStrLn $ show $ MyLib.getScorePart2 input
