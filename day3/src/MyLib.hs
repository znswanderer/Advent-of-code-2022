module MyLib (prioSum, prioSum2) where

import Data.Set (Set, intersection, elemAt)
import qualified Data.Set as Set
import Data.Char (isLower, ord)
import Data.List.Split (chunksOf)

-- Keine explodierenden Krater und keine verletzten Elfen oder Einhörner!
        
findCommon :: String -> Char
findCommon line =
    let 
        [s1, s2] = map Set.fromList $ splitInTwo line
    in 
        elemAt 0 $ intersection s1 s2
  where
    splitInTwo :: String -> [String]
    splitInTwo s = 
        let half = (length s) `div` 2
        in [take half s, drop half s]


priority :: Char -> Int
priority c =
    if isLower c then (ord c) - 96 else (ord c) - 38

prioSum :: String -> Int
prioSum = sum . map priority . map findCommon . lines 

--- part 2

findCommonInRucksacks :: [Set Char] -> Char
findCommonInRucksacks rs = elemAt 0 $ foldr Set.intersection (rs !! 0) (drop 1 rs)

type Badge = Char

badges :: String -> [Badge]
badges = map findCommonInRucksacks . chunksOf 3 . map Set.fromList . lines

prioSum2 :: String -> Int
prioSum2 = sum . map priority . badges


testInput = "vJrwpWtwJgWrhcsFMMfFFhFp\n\
\jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n\
\PmmdzqPrVvPwwTWBwg\n\
\wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n\
\ttgJtRGJQctTZtZT\n\
\CrZsJsPPZsGzwwsLwLmpwMDw"