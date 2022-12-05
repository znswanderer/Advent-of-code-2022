module MyLib (answer) where

import DataStructures
import Parsers

import Text.Parsec (ParseError)

import Data.List (transpose)
import Data.Maybe (catMaybes)
import Debug.Trace
import Control.Monad (foldM)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import System.IO.Unsafe (unsafePerformIO)



-- Getting the text files for interactive ghci usage...
unsafeReadFile = unsafePerformIO . readFile 

row1 = "    [D]    "
row2 = "[N] [C]    "
row3 = "[Z] [M] [P]"
row4 = " 1   2   3 "

-- Yes, I know this can be done easily in a XY coordinate system,
-- but I want to do it in Parsec.

type StackMap = IntMap Stack

constructStacks :: Arrangement -> StackMap
constructStacks (Arrangement rows colNums) = 
    IntMap.fromList $ zip colNums (map catMaybes $ transpose rows)



move :: Instruction -> StackMap -> Maybe StackMap
move (Move n fromCol toCol) sm = do
    moved <- take n <$> IntMap.lookup fromCol sm
    --trace (show moved) (Just 1)
    let sm' = IntMap.adjust (drop n) fromCol sm
    return $ IntMap.adjust ((reverse moved) ++) toCol sm'


runSheet :: Sheet -> Maybe StackMap
runSheet (Sheet arr moves) =
    let sm = constructStacks arr
    in
        foldM (flip move) sm moves

top :: StackMap -> String
top = map head . IntMap.elems

answer :: String -> Maybe String
answer x = do
    let s = case regularParse sheet x of
                Right s  -> s
                Left err -> error $ show err
    let sm = runSheet s
    top <$> sm


