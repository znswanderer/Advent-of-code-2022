module Main where

import System.Environment

import qualified MyLib (findMarker, findMarker2)

main :: IO ()
main = getArgs >>= parse 

parse ["--part1", path] = do
  input <- readFile path
  putStrLn $ show $ MyLib.findMarker input

parse ["--part2", path] = do
  input <- readFile path
  putStrLn $ show $ MyLib.findMarker2 input
