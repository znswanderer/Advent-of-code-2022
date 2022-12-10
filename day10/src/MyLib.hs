module MyLib (part1, part2) where

import Data.List (scanl')


import System.IO.Unsafe (unsafePerformIO)
-- Getting the text files for interactive ghci usage...
unsafeReadFile = unsafePerformIO . readFile 

{-
-- State Machine


type CycleCounter = Int

data Operation = NOOP | ADDX Int

type OperationCounter = Int

type Instruction = (Operation, OperationCounter)

type Register = Int

type CPUState = (CycleCounter, Register)

exec :: CPUState -> Instruction -> [CPUState]
exec (cc, rg) (NOOP, _)   = (cc+1, rg)  :[]
exec (cc, rg) (ADDX n, 1) = (cc+1, rg)  :(exec (cc+1, rg) (ADDX n, 0))
exec (cc, rg) (ADDX n, 0) = (cc+1, rg+n):[]

instruction :: Operation -> Instruction
instruction NOOP     = (NOOP, 0)
instruction (ADDX n) = (ADDX n, 1)

example = [NOOP, ADDX 3, ADDX (-5)]

--run :: CPUState -> [Instruction] -> [CPUState]
--run 

runEx ops = foldM exec (0, 0) (reverse (map instruction ops))



-}

data Pseudo = NewCycle | Incr Int deriving (Eq, Show)

noop = [NewCycle]

addx n = [NewCycle, Incr n]

--newtype CycleCounter = CC Int deriving (Eq, Show)
newtype Register = RG Int deriving (Eq, Show)

--type CPU = (CycleCounter, Register)

run :: Register -> Pseudo -> Register
run r NewCycle = r
run (RG r) (Incr x) = RG (r+x)

example = concat [noop, addx 3, addx (-5)]

start = RG 1


readLine :: String -> [Pseudo]
readLine "noop" = noop
readLine s      = addx $ read $ (words s) !! 1 -- not very safe!

readProgam :: String -> [Pseudo]
readProgam = concat . (map readLine) . lines 

runProgram = scanl' run start

{-
every :: (Int -> Bool) -> [CPU] -> [CPU]
every pred = filter f 
  where
    f (CC c, r) = pred c 

-}

focusCycles :: [Register] -> [(Int, Register)]
focusCycles rs = filter (\(x, _) -> (x - 20) `mod` 40 == 0) $ zip [1..] rs

strength rs = sum $ map (\(c, (RG r)) -> c*r) (focusCycles rs) 



--------------------------

part1 = strength . runProgram . readProgam

part2 = const "part2"