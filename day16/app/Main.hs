module Main where

import System.Environment

import qualified MyLib (part1)
import qualified MyLib2 (part2)

main :: IO ()
main = getArgs >>= parse 

parse ["--part1", path] = do
  input <- readFile path
  putStrLn $ MyLib.part1 input

parse ["--part2", path] = do
  input <- readFile path
  putStrLn $ MyLib2.part2 input
