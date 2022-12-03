module Main where

import System.Environment
import qualified MyLib (prioSum, prioSum2)


main :: IO ()
main = getArgs >>= parse 

parse ["--part1", path] = do
  input <- readFile path
  putStrLn $ show $ MyLib.prioSum input

parse ["--part2", path] = do
  input <- readFile path
  putStrLn $ show $ MyLib.prioSum2 input
