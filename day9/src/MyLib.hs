module MyLib (part1, part2) where

import Data.Traversable (mapAccumL)
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

data Rope = Rope { headPos :: Pos, tailPos :: Pos }
    deriving (Eq, Show, Ord)

start = Rope (1, 1) (1, 1)

data Move = MUp | MDown | MRight | MLeft
    deriving (Eq, Show)

moveHead :: Rope -> Move -> Rope
moveHead (Rope (x, y) t) m = moveHead' m
  where
    moveHead' MUp    = Rope (x, y+1) t
    moveHead' MDown  = Rope (x, y-1) t
    moveHead' MRight = Rope (x+1, y) t
    moveHead' MLeft  = Rope (x-1, y) t

moveTail :: Rope -> Rope
moveTail r@(Rope h@(hx, hy) (tx, ty)) =
    let dx = hx - tx
        dy = hy - ty
    in
        if abs dx > 1 || abs dy > 1 then
            -- I like to move it
            Rope h (tx + pmOne dx, ty + pmOne dy)
        else
            r

pmOne :: Int -> Int
pmOne = (max (-1)) . (min 1) 

moveRope :: Rope -> Move -> Rope
moveRope r = moveTail . (moveHead r)

mright n = replicate n MRight
mleft n = replicate n MLeft
mup n = replicate n MUp
mdown n = replicate n MDown


example = concat [mright 4, mup 4, mleft 3, mdown 1, mright 4, mdown 1, mleft 5, mright 2]


-- mapAccumL :: forall t s a b. Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b) 
-- I still have problems writng point free with more than one argument
makeMoves :: Rope -> [Move] -> [Rope]
makeMoves rp mvs = snd $ makeMoves' rp mvs
  where
    runner r mv = (\x -> (x, x)) $ (moveRope r mv)
    makeMoves'  = mapAccumL runner 

numVisited :: [Move] -> Int
numVisited = length . Set.fromList  . (map tailPos) . (makeMoves start)


buildMoves :: String -> [Move]
buildMoves = concat . map buildMoves' . lines
  where
    buildMoves' :: String -> [Move]
    buildMoves' ('R':s) = mright $ read s
    buildMoves' ('L':s) = mleft $ read s
    buildMoves' ('U':s) = mup $ read s
    buildMoves' ('D':s) = mdown $ read s

part1 = numVisited . buildMoves

part2 = part1