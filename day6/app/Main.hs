module Main where

import System.Environment

import qualified MyLib (findMarker)

main :: IO ()
main = getArgs >>= parse 

parse ["--part1", path] = do
  input <- readFile path
  putStrLn $ show $ MyLib.findMarker input

