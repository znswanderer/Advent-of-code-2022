module MyLib (part1, part2) where

import qualified Data.Map.Strict as Map

import Data.List (foldl', find, replicate, sort)
import Data.List.Extra (firstJust)
import Data.Char (ord)
import Control.Monad ( guard, (<=<) ) 
import Data.Maybe (fromJust, catMaybes)

import Data.Set (Set)
import qualified Data.Set as Set

{-
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad.ListT (ListT)
import Data.Functor.Identity (Identity(..))
-}

import System.IO.Unsafe (unsafePerformIO)
-- Getting the text files for interactive ghci usage...
unsafeReadFile = unsafePerformIO . readFile 

exStr = "Sabqponm\n\
\abcryxxl\n\
\accszExk\n\
\acctuvwj\n\
\abdefghi"

type Pos = (Int, Int)

type Height = Int

type HeightMap = Map.Map Pos Char

height :: Char -> Height
height 'S' = height 'a'
height 'E' = height 'z'
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
                        Map.insert (colN, rowN) val m')
                     m (zip [1..] row)
            )
            Map.empty (zip [1..] rows)

findPos :: Char -> HeightMap -> Pos
findPos c m = 
    let
        Just (pos, _) = find (\(k, c') -> c == c') (Map.toList m)
    in
        pos


startPos = findPos 'S'
endPos = findPos 'E'

-----------------------

type DistanceMap = Map.Map Pos Path


heightAtPos :: HeightMap -> Pos -> Height
heightAtPos m pos = height $ fromJust $ Map.lookup pos m

type Path = [Pos]



singleStep :: HeightMap -> Path -> [Path]
singleStep m path@(p:ps) = do
    let (x, y) = p
    let h = heightAtPos m p
    p' <- [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    guard (not $ p' `elem` path)
    guard (Map.member p' m)
    let h' = heightAtPos m p'
    guard (h' - h <= 1)
    -- pruning: Already a shorter path to new postion?
    let path' = p':path
    --case Map.lookup p' dm of 
    --    Nothing -> 
    return path'


-- type PathList a = ListT (State.State DistanceMap) a
-- type PathList a = ListT Identity a


singleStepPM :: Int -> HeightMap -> DistanceMap -> Path -> [Path]
singleStepPM maxLength m dm path@(p:ps) = do
    let (x, y) = p
    let h = heightAtPos m p
    p' <- [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    guard (not $ p' `elem` path)
    guard (Map.member p' m)
    let h' = heightAtPos m p'
    guard (h' - h <= 1)
    -- pruning: Already a shorter path to new postion?
    let path' = p':path
    guard (length path' <= maxLength + 1)
    case Map.lookup p' dm of
        -- ATTENTION: return [] is NOT []!
        Just path'' -> if length path'' <= length path' then [] else return path'
        Nothing     -> return path'

-- after each step: if a position can be reached by two path, choose
-- one path to keep the tree of possible paths short
pruneTree :: [Path] -> [Path]
pruneTree [] = []
pruneTree ps = Map.elems $ Map.fromList (map (\path@(p:ps) -> (p, path) ) ps)

insertPathList :: DistanceMap -> [Path] -> DistanceMap
insertPathList dm ps = foldl' (\m p -> Map.insert (head p) p m) dm ps

type Travel = (DistanceMap, [Path])

completeStep :: Int -> HeightMap -> Travel -> Travel
completeStep maxLength hm (dm, ps) =
    let
        ps'  = ps >>= singleStepPM maxLength hm dm
        ps'' = pruneTree ps'
        dm'  = insertPathList dm ps''
    in
        (dm', ps'')



findSolution :: Int -> HeightMap -> Travel -> Maybe Path
findSolution maxLength hm tv@(dm, ps) = 
    let
        end = endPos hm
    in 
        if ps == [] then Nothing
        else
            if (length $ head ps) >= maxLength + 1 then Nothing 
            else
                case Map.lookup end dm of
                    Just sol -> Just sol
                    Nothing  -> findSolution maxLength hm $ completeStep maxLength hm tv

-- travel :: HeightMap -> Travel -> [Travel]
-- travel m tv = (completeStep m tv):[tv]


-- breadth first list monad?

-- from http://learnyouahaskell.com/for-a-few-monads-more
inMany :: Int -> HeightMap -> Path -> [Path]
inMany x m path = return path >>= foldr (<=<) return (replicate x (singleStep m))

{-
newSearchDepth :: HeightMap -> Path -> Either Path [Path]
newSearchDepth m p =
    let
        ps = singleStep m p
        end = endPos m
    in 
        case find (\x -> head x == end) ps of
            Just p' -> Left p'
            Nothing -> Right $ pruneTree ps
-}

solution :: HeightMap -> Maybe Path
solution m = 
    let
        start    = startPos m
        depths   = [25..] -- must be at least 25 long because z-a = 25
        runs     = zip depths (map (\n -> inMany n m [start]) depths)
    in
        firstJust (\(n, ps) -> foundEnd ps) runs
      where
        end          = endPos m
        foundEnd ps' = find (\ps'' -> head ps'' == end) ps'

heightPath :: HeightMap -> Path -> [Height]
heightPath m p = map height $ map (\pos -> fromJust $ Map.lookup pos m) p


part1 s = 
    let 
        m    = makeMap s
        tv   = (Map.empty, [[startPos m]]) :: Travel
    in case findSolution 1000 m tv of 
        Just path ->
                    "Found path of length " ++ (show ((length path) - 1)) ++ " path: " ++ (show $ reverse path)
        Nothing -> "no solution found"


--- part 2

allZeroPositions :: HeightMap -> [Pos]
allZeroPositions m = map (\(p, _) -> p) $ filter (\(pos, h) -> h == 0)  $ map (\(k, v) -> (k, height v)) $ Map.toList m


trails :: HeightMap -> [Maybe Path]
trails m = 
    next (length $ Map.keys m) (allZeroPositions m)

  where
    next maxL (s:[]) = [findSolution maxL m (Map.empty, [[s]])]
    next maxL (s:ss) = case findSolution maxL m (Map.empty, [[s]]) of
        Just path -> (Just path):(next (min (maxL) (length path)) ss)
        Nothing   -> Nothing:(next maxL ss)


part2 s = 
    let 
        m         = makeMap s
        trs       = catMaybes $ trails m
        numTrails = map (\x -> (length x) -1) trs
        shortest  = head $ sort $ numTrails
    in
        "Found " ++ (show $ length trs) ++ " paths. The shortest has length " ++ (show shortest)



