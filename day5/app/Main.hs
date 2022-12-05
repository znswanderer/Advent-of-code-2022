module Main where

import System.Environment

import qualified MyLib (answer)

main :: IO ()
main = getArgs >>= parse 

parse ["--part1", path] = do
  input <- readFile path
  putStrLn $ show $ MyLib.answer input

