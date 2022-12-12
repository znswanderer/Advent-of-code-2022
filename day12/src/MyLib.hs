module MyLib (part1, part2) where

import qualified Data.Map.Strict as Map

import Data.List (foldl', find)
import Data.Char (ord)

exStr = "Sabqponm\n\
\abcryxxl\n\
\accszExk\n\
\acctuvwj\n\
\abdefghi"

type Pos = (Int, Int)

type Height = Int

type HeightMap = Map.Map Pos Height

height :: Char -> Height
height c = ord c - ord 'a'

startHeight = height 'S'
endHeight = height 'E'

makeMap :: String -> HeightMap
makeMap s = 
    let 
        rows = lines s
    in
        foldl' 
            (\m  (rowN, row) -> 
                foldl' 
                    (\m' (colN, val) -> 
                        Map.insert (rowN, colN) (height val) m')
                     m (zip [1..] row)
            )
            Map.empty (zip [1..] rows)

findHeight :: Height -> HeightMap -> Pos
findHeight h m = 
    let
        Just (pos, _) = find (\(k, v) -> v == h) (Map.toList m)
    in
        pos


startPos = findHeight startHeight
endPos = findHeight endHeight

-- foldl' (b -> a -> b) b (t a)

part1 = const "part1"

part2 = const "part2"