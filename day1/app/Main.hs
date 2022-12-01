module Main where

import System.Environment
import qualified MyLib (findHighCalElf, top3Cal)

main :: IO ()
main = getArgs >>= parse 

parse ["--max", path] = do
  putStr "Highest calories carried by one elf: "
  input <- readFile path
  putStrLn $ show $ MyLib.findHighCalElf input

parse ["--top3", path] = do
  putStr "Calories carried by top 3 elves: "
  input <- readFile path
  putStrLn $ show $ MyLib.top3Cal input

