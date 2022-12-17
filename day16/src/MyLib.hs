module MyLib (part1, part2) where

import Data.Maybe (fromJust, catMaybes)
import Data.List.HT (removeEach)
import Data.List (sort, foldl')
import Control.Monad (guard)

import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar, string, char, digit, oneOf, endOfLine, letter, spaces, alphaNum, noneOf, satisfy)
import Text.Parsec.Combinator (many1, sepBy1, endBy, lookAhead, sepBy)
import Text.Parsec (parse, ParseError, eof, (<|>), skipMany, manyTill, try, many)

import qualified Data.Map.Strict as Map
import qualified Data.HashSet as H
import Data.Hashable

import Data.Graph.AStar

import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace
--trace (show moved) (Just 1)

import System.IO.Unsafe (unsafePerformIO)
-- Getting the text files for interactive ghci usage...
unsafeReadFile = unsafePerformIO . readFile 

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

num :: Parser Int
num = do
    n <- many1 digit
    return (read n)


parseValve :: Parser Valve
parseValve = do
    string "Valve "
    name <- many1 letter
    string " has flow rate="
    rate <- num
    (try $ string "; tunnels lead to valves ") <|> (string "; tunnel leads to valve ")
    exits <- (many1 letter) `sepBy` (string ", ")
    return $ Valve name rate exits


parseFile :: Parser ValveMap
parseFile = do 
    valveList <- parseValve `sepBy` endOfLine 
    return $ Map.fromList $ map (\v -> (vName v, v)) valveList

-----------------------------------



type ValveMap = Map.Map String Valve

type ValveName = String

data Valve = Valve {
    vName       :: ValveName,  
    vRate       :: Int, 
    vExits      :: [String]
} deriving (Eq, Show, Ord)

type ClosedValveSet = Set String


--- PART A
-- find shortest path from valve to valve

valveForString :: ValveMap -> ValveName -> Valve
valveForString m s = fromJust $ Map.lookup s m

neighbours :: ValveMap -> ValveName -> H.HashSet ValveName
neighbours m vn = H.fromList $ vExits (valveForString m vn)

-- https://github.com/david-crespo/aoc/blob/main/2022/hs-aoc/src/Day12.hs
getPath :: ValveMap -> ValveName -> ValveName -> [ValveName]
getPath m start end = fromJust $ aStar (neighbours m) (\_ _ -> 1) (\_ -> 0) (== end) start

distance :: ValveMap -> ValveName -> ValveName -> Int
distance m start end = length $ getPath m start end

-- distance map

pairs :: Ord a =>  [a] -> [Set.Set a]
pairs (x:[]) = []
pairs (x:xs) = (map (\x' -> Set.fromList [x, x']) xs) ++ (pairs xs)


startValveName = "AA"


distanceMap :: ValveMap -> Map.Map (Set.Set ValveName) Int
distanceMap m = 
    let
        vps = pairs $ [startValveName] ++ (map vName $ validValves m)
    in
        Map.fromList $ map (\p -> (p, dst (Set.toList p))) vps
    where
        dst (a:b:[]) = (distance m a b) + 1 -- +1 for opening valve

dst :: Map.Map (Set.Set ValveName) Int -> ValveName -> ValveName -> Int
dst dm x y = fromJust $ Map.lookup (Set.fromList [x,y]) dm

pathLength dm path = sum $ map (uncurry (dst dm)) $ zip (drop 1 path) path


-- all possible paths in a given time
possiblePath :: ValveMap -> (Int, ValveName) -> Int -> [[(Int, ValveName)]]
possiblePath m start maxTime =
        suggi dm [start] (startUnivisted m)
    where
        dm = distanceMap m
        startUnivisted m = map vName $ validValves m
        suggi dm prev unvisited = do
            (x, xs) <- removeEach unvisited
            let (t, pos) = head prev
            let t' = t + dst dm pos x
            guard (t' <= maxTime)
            let prev' = (t', x):prev
            prev':suggi dm prev' xs
    

pathValue m path = sum $ map (\(t, x) -> (maxMinute -t) * (vRate $ valveForString m x)) path

bestPath m = head $ reverse $ sort $ map (\p -> (pathValue m p, p) ) $ possiblePath m (0, "AA") 30


validValves :: ValveMap -> [Valve]
validValves m = filter (\v -> vRate v > 0) (Map.elems m)

maxMinute = 30 :: Int

part1 s =
    let
        Right m = regularParse parseFile s
    in
        show $ bestPath m

part2 s = "bbb"
