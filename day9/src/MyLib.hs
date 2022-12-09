module MyLib (part1, part2) where

import Data.Traversable (mapAccumL)
import qualified Data.Set as Set
import Data.Foldable (foldl')
import Data.List (scanl')

type Pos = (Int, Int)

data Rope = Rope { headPos :: Pos, tailPos :: Pos }
    deriving (Eq, Show, Ord)

start = Rope (1, 1) (1, 1)

data Move = MUp | MDown | MRight | MLeft
    deriving (Eq, Show)

moveHead :: Rope -> Move -> Rope
moveHead (Rope h t) m = Rope (movePos h m) t

movePos :: Pos -> Move -> Pos
movePos (x, y) MUp    = (x, y+1)
movePos (x, y) MDown  = (x, y-1)
movePos (x, y) MRight = (x+1, y)
movePos (x, y) MLeft  = (x-1, y)


moveTail :: Rope -> Rope
moveTail (Rope h t) = Rope h (follow h t)

follow :: Pos -> Pos -> Pos
follow (hx, hy) t@(tx, ty) =
    let dx = hx - tx
        dy = hy - ty
    in
        if abs dx > 1 || abs dy > 1 then
            (tx + pmOne dx, ty + pmOne dy)
        else
            t

pmOne :: Int -> Int
pmOne = (max (-1)) . (min 1) 

moveRope :: Rope -> Move -> Rope
moveRope r = moveTail . (moveHead r)

mright n = replicate n MRight
mleft n = replicate n MLeft
mup n = replicate n MUp
mdown n = replicate n MDown


example = concat [mright 4, mup 4, mleft 3, mdown 1, mright 4, mdown 1, mleft 5, mright 2]


makeMoves :: (a -> Move -> a) -> a -> [Move] -> [a]
makeMoves = scanl'

numVisited :: [Move] -> Int
numVisited = length . Set.fromList  . (map tailPos) . (makeMoves moveRope start)


buildMoves :: String -> [Move]
buildMoves = concat . map buildMoves' . lines
  where
    buildMoves' :: String -> [Move]
    buildMoves' ('R':s) = mright $ read s
    buildMoves' ('L':s) = mleft $ read s
    buildMoves' ('U':s) = mup $ read s
    buildMoves' ('D':s) = mdown $ read s

part1 = numVisited . buildMoves

-- ---------------- PART 2 -------------------

type LongRope = [Pos]

-- foldl' :: (b -> a -> b) -> b -> t a -> b 
moveLongRope :: LongRope -> Move -> LongRope
moveLongRope (p:ps) mv = 
    reverse $ foldl' (\hs p -> (follow (head hs) p):hs) [movePos p mv] ps

longStart = replicate 10 (0,0)

numVisitedLong :: [Move] -> Int
numVisitedLong = length . Set.fromList  . (map (head . reverse)) . (makeMoves moveLongRope longStart)

part2 = numVisitedLong . buildMoves