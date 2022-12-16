module MyLib (part1, part2) where

import Data.Maybe (fromJust, catMaybes)
import Data.List.HT (removeEach)
import Data.List (sort)

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
} deriving (Eq, Show)

type ClosedValveSet = Set String


--- PART A
-- find shortest path from valve to valve

valveForString :: ValveMap -> ValveName -> Valve
valveForString m s = fromJust $ Map.lookup s m

neighbours :: ValveMap -> ValveName -> H.HashSet ValveName
neighbours m vn = H.fromList $ vExits (valveForString m vn)

-- https://github.com/david-crespo/aoc/blob/main/2022/hs-aoc/src/Day12.hs
getPath :: ValveMap -> ValveName -> ValveName -> Maybe [ValveName]
getPath m start end = aStar (neighbours m) (\_ _ -> 1) (\_ -> 0) (== end) start

distance :: ValveMap -> ValveName -> ValveName -> Int
distance m start end = length $ fromJust $ getPath m start end


-- PART B
-- I build a small graph only consisting of the valid valuess

validValves :: ValveMap -> [Valve]
validValves m = filter (\v -> vRate v > 0) (Map.elems m)

maxFlow :: ValveMap -> Int
maxFlow = sum . (map vRate) . validValves


data Vertix = Vertix {
    deficitFlow :: Int,
    minute :: Int,
    position :: ValveName,
    closedValves :: [ValveName]
} 
    deriving (Eq, Show, Ord)

instance Hashable Vertix where
    hashWithSalt n v = hashWithSalt n $ show v

cost :: Vertix -> Vertix -> Int
cost start end = ((minute end) - (minute start)) * (deficitFlow start)
-- cost start end = (maxMinute - (minute end)) * ((deficitFlow end) - (deficitFlow start))


suggest :: ValveMap -> Vertix -> H.HashSet Vertix
suggest m start = H.fromList $ filter (\v -> minute v < maxMinute) $ map 
        (\(nextValve, restClosed) -> makeVertix m start nextValve restClosed)
        (removeEach (closedValves start))

makeVertix :: ValveMap -> Vertix -> ValveName -> [ValveName] -> Vertix
makeVertix m oldV name closedValves = 
    let v = valveForString m name
        newFlow  = (deficitFlow oldV) - (vRate v)
        moveTime = distance m (position oldV) name
    in
        Vertix newFlow ((minute oldV) + moveTime + 1) name closedValves

shortestReachable :: ValveMap -> Vertix -> Int
shortestReachable m v = head $ sort $ map (\x -> distance m x (position v)) (closedValves v)

startAt :: ValveMap -> ValveName -> Vertix
startAt m name = Vertix (maxFlow m) 0 name (map vName $ validValves m)
--startAt m name = Vertix 0 0 name (map vName $ validValves m)

-- removeEach :: [a] -> [(a, [a])] 


maxMinute = 30 :: Int

goal :: ValveMap -> Vertix -> Bool
goal m v = (minute v >= maxMinute) || (closedValves v == []) || ((minute v) + (shortestReachable m v) > maxMinute)

part1 s = 
    let 
        Right m = regularParse parseFile s
        Just path = aStar (suggest m) cost (\_ -> 0) (goal m) (startAt m "AA")
        flow0 = maxFlow m
        times = (map (\v -> minute v + 1) path) ++ [maxMinute+1]
        dt = map (\(x,y) -> x -y) $ zip (drop 1 times) times 
        flows = map (\v -> flow0 - deficitFlow v) path
        vals = map (\(t, fl) -> t*fl) $ zip dt flows
    in 
        (
            "flows " ++ (show flows) ++ "\n" ++
            "times " ++ (show times) ++ "\n" ++
            "vals " ++ (show vals) ++ " sum " ++ (show $ sum vals) 
            ++ "\npath is\n" ++ (unlines $ map show $ path)
        )


{-
-- PART B
-- find all path to all closed valid valves and calculate the cost
-- valid valve are all valves that have a rate > 0


data Vertix = Vertix {
    xMinute          :: Int,
    xCurrentPosition :: ValveName,
    xOpenValves      :: [ValveName],
    xClosedValves    :: [ValveName]
} deriving (Eq, Show, Ord)

instance Hashable Vertix where
    hashWithSalt n v = hashWithSalt n $ show v

openNext :: ValveMap -> Vertix -> H.HashSet Vertix
openNext m (Vertix t cur opened closed) = H.fromList $ map (\(toOpen, closed') -> 
                (Vertix (t + distance m cur toOpen)  
                    toOpen 
                    (toOpen:opened) 
                    closed')) 
                (removeEach closed)

-}







------------------

{-


data World = World {
    tMinute       :: Int,
    tLastStep     :: Step,
    tClosedValves :: ClosedValveSet,
    tRestFlow     :: Int,  -- the flow still left to open
    tFlow         :: Int, -- the combined flow of all opened valves
    tFlowAccum    :: Int  -- the accumulated flow
}
    deriving (Eq, Show, Ord)


data Step = MoveToValve String | OpenValve String
    deriving (Eq, Show, Ord)

handledValve :: Step -> String
handledValve (MoveToValve s) = s
handledValve (OpenValve s) = s

instance Hashable World where
    hashWithSalt n ts = hashWithSalt n $ show ts


-- Graph

maxMinute = 30 :: Int

nextStepsAfter :: ValveMap -> World -> H.HashSet World
nextStepsAfter m (World t lastMove closedValves restFlow curFlow acc) =
    let 
        valveName = (handledValve lastMove)
    in 
        case Map.lookup valveName m of
            Nothing -> error $ "Illegal valve name " ++ valveName  -- Should never happen
            -- A list of possible next step is going to one of the neighbour valves or try to open this valve
            Just v  -> H.fromList $ (nextMoves v) ++ (
                if 
                    ((vRate v) > 0) && (Set.member valveName closedValves) 
                then 
                    -- A possible move is to open the current valve and delete it from the set of closed valves
                    [World 
                        (t+1) 
                        (OpenValve valveName) 
                        (Set.delete valveName closedValves) 
                        (restFlow - (vRate v))
                        (curFlow + (vRate v))
                        (acc + curFlow)
                    ] 
                else
                    -- The current valve is already open 
                    [])
    where
        -- next moves from this valve to its neighbours
        nextMoves v = map (\s -> World (t+1) s closedValves restFlow curFlow (acc + curFlow)) $ map MoveToValve (vExits v)

-- distance function for A*
distance :: World -> World -> Int
distance _ (World _ _ _ restFlow _ _) = restFlow

-- heuristic distance for A*
heuristic :: ValveMap -> World -> Int
heuristic m (World _ (MoveToValve name) closedValves restFlow _ _) =
    -- what will I gain by opening this valve and all its neighbours?
    let
        Just v = Map.lookup name m
        closedNb = filter (\x -> Set.member x closedValves) (vExits v)
        neighbours = catMaybes $ map (\x -> Map.lookup x m) closedNb
        flowOfNeighbours = sum $ map vRate neighbours
    in
        --restFlow - (vRate v) - (trace ("Moveto " ++ name ++ " flowOfNeighbours " ++ show flowOfNeighbours) flowOfNeighbours)
        restFlow - (vRate v) - flowOfNeighbours
heuristic m (World _ _ _ restFlow _ _) = restFlow

-- Start
startWorld :: ValveMap -> World
startWorld m = 
    let
        maxFlow = sum $ map vRate $ Map.elems m
    in
        World 0 (MoveToValve "AA") (Set.fromList $ Map.keys m) maxFlow 0 0


-- This is the goal condition for the A* algorithm
goal :: World -> Bool
goal (World t _ closedValves _ _ _) = (t == maxMinute) -- || (Set.null closedValves)


{-
Is the list monad lazy?

testLazy = do
    x <- [1,2,3] :: [Int]
    y <- [10, 11, 12] :: [Int]
    return $ (trace (show x) x) + (trace (show y) y)

part1 = const $ show $ take 4 $ testLazy

Returns:

1
10
11
12
2
10
"[11,12,13,12]"
--> yes, the list moand is lazy ;-)
-}

part1 s = 
    let
        Right m = regularParse parseFile s
        Just path = aStar (nextStepsAfter m) distance (heuristic m) goal (startWorld m)
        lineOutput (World t lastMove closedValves restFlow curFlow acc) =
            let
                openValves = Set.difference (Set.fromList $ Map.keys m) closedValves
            in
                (
                    "== Minute " ++ (show t) ++ " ==\n" ++
                    "Open valves: " ++ (show openValves) ++ " releasing " ++ (show curFlow) ++ " pressure\n" ++
                    (show lastMove) ++ " accumulated flow: " ++ (show acc) ++ "\n"
                )
    in
        unlines $ map lineOutput path
-}  

-- part1 = const "part1"
part2 = const "part2"