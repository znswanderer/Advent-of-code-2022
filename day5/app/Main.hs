module Main where

import System.Environment

import qualified MyLib (answer, answer2)

main :: IO ()
main = getArgs >>= parse 

parse ["--part1", path] = do
  input <- readFile path
  putStrLn $ show $ MyLib.answer input

parse ["--part2", path] = do
  input <- readFile path
  putStrLn $ show $ MyLib.answer2 input