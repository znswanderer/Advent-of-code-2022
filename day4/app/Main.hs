module Main where

import System.Environment

import qualified MyLib (sumOverlaps)

main :: IO ()
main = getArgs >>= parse 

parse ["--part1", path] = do
  input <- readFile path
  case MyLib.sumOverlaps input of
    Right  n -> putStrLn $ "Sum of overlaps is " ++ show n
    Left err -> putStrLn $ show $ err

