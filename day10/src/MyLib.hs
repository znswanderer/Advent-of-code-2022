module MyLib (part1, part2) where

import Data.List (scanl')
import Data.List.Split (chunksOf)


import System.IO.Unsafe (unsafePerformIO)
-- Getting the text files for interactive ghci usage...
unsafeReadFile = unsafePerformIO . readFile 


data Pseudo = NewCycle | Incr Int deriving (Eq, Show)

noop = [NewCycle]

addx n = [NewCycle, Incr n]

type Register = Int

run :: Register -> Pseudo -> Register
run r NewCycle = r
run r (Incr x) = r+x

example = concat [noop, addx 3, addx (-5)]

start = 1


readLine :: String -> [Pseudo]
readLine "noop" = noop
readLine s      = addx $ read $ (words s) !! 1 -- not very safe!

readProgram :: String -> [Pseudo]
readProgram = concat . (map readLine) . lines 

runProgram = scanl' run start

focusCycles :: [Register] -> [(Int, Register)]
focusCycles rs = filter (\(x, _) -> (x - 20) `mod` 40 == 0) $ zip [1..] rs

strength rs = sum $ map (\(c, r) -> c*r) (focusCycles rs) 



--------------------------

part1 = show . strength . runProgram . readProgram

-------- PART 2

part2 :: String -> String
part2 prg = 
    let 
        x = zip (concat $ repeat [0..39]) $ runProgram $ readProgram prg
    in 
        unlines $ chunksOf 40 $ map (\(px, sp) -> if abs (px - sp) < 2 then '#' else '.') x
