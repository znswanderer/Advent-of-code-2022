module MyLib (part1, part2) where

import Data.Traversable (mapAccumL)
import qualified Data.Set as Set
import Data.Foldable (foldl')
import Data.List (scanl')

type Pos = (Int, Int)

type Rope = [Pos]

data Move = MUp | MDown | MRight | MLeft
    deriving (Eq, Show)

mright n = replicate n MRight
mleft n = replicate n MLeft
mup n = replicate n MUp
mdown n = replicate n MDown

example = concat [mright 4, mup 4, mleft 3, mdown 1, mright 4, mdown 1, mleft 5, mright 2]


movePos :: Pos -> Move -> Pos
movePos (x, y) MUp    = (x, y+1)
movePos (x, y) MDown  = (x, y-1)
movePos (x, y) MRight = (x+1, y)
movePos (x, y) MLeft  = (x-1, y)


follow :: Pos -> Pos -> Pos
follow (hx, hy) t@(tx, ty) =
    let dx = hx - tx
        dy = hy - ty
    in
        if abs dx > 1 || abs dy > 1 then (tx + pmOne dx, ty + pmOne dy) else t

-- Constrict to interval [-1, 1]
pmOne :: Int -> Int
pmOne = (max (-1)) . (min 1) 

makeMoves :: (a -> Move -> a) -> a -> [Move] -> [a]
makeMoves = scanl'


buildMoves :: String -> [Move]
buildMoves = concat . map buildMoves' . lines
  where
    buildMoves' :: String -> [Move]
    buildMoves' ('R':s) = mright $ read s
    buildMoves' ('L':s) = mleft $ read s
    buildMoves' ('U':s) = mup $ read s
    buildMoves' ('D':s) = mdown $ read s

-- foldl' :: (b -> a -> b) -> b -> t a -> b 
moveRope :: Rope -> Move -> Rope
moveRope (p:ps) mv = 
    reverse $ foldl' (\hs p -> (follow (head hs) p):hs) [movePos p mv] ps

numVisited :: Rope -> [Move] -> Int
numVisited rp = length . Set.fromList  . (map (head . reverse)) . (makeMoves moveRope rp)


rope2 = replicate 2 (0,0)

part1 = (numVisited rope2) . buildMoves


-- ---------------- PART 2 -------------------

rope10 = replicate 10 (0,0)


part2 = (numVisited rope10) . buildMoves