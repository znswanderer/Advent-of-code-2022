module Main where

import System.Environment

import qualified MyLib (sumFullOverlaps, sumPartialOverlaps)

main :: IO ()
main = getArgs >>= parse 

parse ["--part1", path] = do
  input <- readFile path
  case MyLib.sumFullOverlaps input of
    Right  n -> putStrLn $ "Sum of fullOverlaps is " ++ show n
    Left err -> putStrLn $ show $ err

parse ["--part2", path] = do
  input <- readFile path
  case MyLib.sumPartialOverlaps input of
    Right  n -> putStrLn $ "Sum of partial overlaps is " ++ show n
    Left err -> putStrLn $ show $ err