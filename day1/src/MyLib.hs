module MyLib (findHighCalElf, top3Cal) where

import Data.String.Utils (strip)
import Data.List.Utils (split)
import Data.List (sort)

findHighCalElf :: String -> Int
findHighCalElf input = 
    maximum $ caloriesPerElf input

top3Cal :: String -> Int
top3Cal input = 
    let revSorted = reverse $ sort $ caloriesPerElf input
    in 
        sum $ take 3 revSorted


caloriesPerElf :: String -> [Int]
caloriesPerElf input =
    let strippedLines = map strip $ lines input
        grouped = split [""] strippedLines
    in
        map sumLines grouped
  where
    sumLines :: [String] -> Int
    sumLines lines = 
        sum $ map read lines
