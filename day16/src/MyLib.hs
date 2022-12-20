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

valveForString :: ValveMap -> ValveName -> Valve
valveForString m s = fromJust $ Map.lookup s m


validValves :: ValveMap -> [Valve]
validValves m = filter (\v -> vRate v > 0) (Map.elems m)


neighbours :: ValveMap -> ValveName -> H.HashSet ValveName
neighbours m vn = H.fromList $ vExits (valveForString m vn)

-- https://github.com/david-crespo/aoc/blob/main/2022/hs-aoc/src/Day12.hs
getPath :: ValveMap -> ValveName -> ValveName -> [ValveName]
getPath m start end = fromJust $ aStar (neighbours m) (\_ _ -> 1) (\_ -> 0) (== end) start

type PathMap = Map.Map (ValveName, ValveName) [ValveName]

distanceMap :: ValveMap -> PathMap
distanceMap m = 
    let
        valves = [startValveName] ++ (map vName $ validValves m)
    in
        Map.fromList $ map (\(a, b) -> ((a, b), getPath m a b)) (pairs valves)
    where
        pairs xs = [(a,  b) | a <- xs, b <- xs, not (a == b) ]

pathFromMap :: PathMap -> ValveName -> ValveName -> [ValveName]
pathFromMap pm x y = fromJust $ Map.lookup (x, y) pm


---------------------------------------------

data Step = Wait | Forward ValveName | OpenValve ValveName | NewTarget ValveName
    deriving (Eq, Show, Ord)

type Time = Int
type ClosedValves = [ValveName]
type Position = ValveName
-- type PathToTarget = [ValveName]

data Target = Target ValveName [ValveName]
    deriving (Eq, Show, Ord)

data Actor = Actor Position (Maybe Target)
    deriving (Eq, Show, Ord)

data WorldState = World Time ClosedValves Step Actor
    deriving (Eq, Show, Ord)

instance Hashable WorldState where
    hashWithSalt n = (hashWithSalt n). show

target :: Actor -> Maybe Target
target (Actor _ tg) = tg

actor :: WorldState -> Actor
actor (World _ _ _ a) = a

nextStep :: ValveMap -> PathMap -> [ValveName] -> WorldState -> H.HashSet WorldState
nextStep m pm tabus w@(World t closed step actor) = 
    let 
        -- thisValve = valveForString m pos
        --openValve = if (vRate thisValve > 0) && (pos `elem` closed) then [OpenValve pos] else []
        --moves = map MoveTo $ vExits thisValve

        -- steps = [Wait] ++ openValve ++ moves
        -- steps Nothing     = trace ("New Target at " ++ pos ++ " closed: " ++ (show closed) ) $ map NewTarget closed
        -- steps (Just []) = trace ("OpenValve " ++ pos) $ [OpenValve pos]
        allowedTargets = filter (not . (`elem` tabus)) closed

        steps (Actor pos Nothing) = map NewTarget allowedTargets
        steps (Actor pos (Just (Target target []))) = [OpenValve pos] -- we are at target, so open it!
        steps (Actor pos (Just (Target target (x:xs)))) = [Forward x]

    in
        H.fromList $! map (\s -> runStep pm w s) (steps actor)

moveForward :: Actor -> Actor
moveForward (Actor pos (Just (Target tn (x:xs)))) = Actor x (Just $ Target tn (xs))

clearTarget :: Actor -> Actor 
clearTarget (Actor pos _) = Actor pos Nothing

newTarget :: PathMap -> ValveName -> Actor -> Actor
newTarget pm v (Actor pos _) = Actor pos (Just $ Target v (pathFromMap pm pos v))

runStep :: PathMap -> WorldState -> Step -> WorldState
runStep pm (World t closed _ actor) (Forward v) = World (t+1) closed (Forward v) (moveForward actor) 
runStep pm (World t closed _ actor) (OpenValve v) = World (t+1) (filter (not . (==v)) closed) (OpenValve v) (clearTarget actor)
runStep pm (World t closed _ actor) (NewTarget v) = 
    let 
        act@(Actor _ (Just (Target _ (x:xs)))) = newTarget pm v actor
        w' = World t closed (NewTarget v) act
    in
        runStep pm w' (Forward x)
runStep pm (World t closed _ actor) Wait = World (t+1) closed Wait actor

{-
runStep pm (World t closed pos _ _ _) (NewTarget v)   = 
    let
        -- (x:xs) = trace ("getPath from " ++ pos ++ " to " ++ v ++ " is " ++ (show $ getPath m pos v)) getPath m pos v
        (x:xs) = pathFromMap pm pos v
    in
        World (t+1) closed x (NewTarget v) xs (Just v)
-}

time :: WorldState -> Int
time (World t closed step actor) = t

closed :: WorldState -> ClosedValves
closed (World t closed step actor) = closed

step :: WorldState -> Step
step (World t closed step actor) = step

closedPressure :: ValveMap -> ClosedValves -> Int
closedPressure m closed = sum $ (map (vRate . (valveForString m)) closed)

-- startWorld m = World 0 (map vName $ validValves m) startValveName (MoveTo startValveName) [] Nothing
startWorld m t = World t (map vName $ validValves m) Wait (Actor startValveName Nothing)


cost :: ValveMap -> WorldState -> WorldState -> Int
cost m start end = ((time end) - (time start)) * (closedPressure m $ closed end)

goal :: WorldState -> Bool
goal (World t closed step actor) = (t == maxMinute) || (null closed)

solutionValue :: ValveMap -> [WorldState] -> Int
solutionValue m ws =
    sum $ map (uncurry (*)) $ solutionValue' m ws
  where 
    solutionValue' m ws = map (\w -> (maxMinute - time w, openedPressure $ step w)) ws
    openedPressure (OpenValve vn) = vRate $ valveForString m vn
    openedPressure _              = 0




startValveName = "AA"
maxMinute = 30 :: Int


part1 s =
    let
        Right m = regularParse parseFile s
        start = startWorld m 0
        pm = distanceMap m
        Just path = aStar (nextStep m pm []) (cost m) (\x -> 0) goal start
    in
        show $ solutionValue m path


-------------------
-- PART 2


data WorldState2 = World2 Time ClosedValves Step Step Actor Actor
    deriving (Eq, Show, Ord)

instance Hashable WorldState2 where
    hashWithSalt n = (hashWithSalt n). show

smallWorld1 :: WorldState2 -> WorldState
smallWorld1 (World2 t closed s1 s2 a1 a2) = World t closed s1 a1

smallWorld2 :: WorldState2 -> WorldState
smallWorld2 (World2 t closed s1 s2 a1 a2) = World t closed s2 a2


largeWorld :: WorldState -> WorldState -> WorldState2
largeWorld w1@(World t1 closed1 s1 a1) w2@(World t2 closed2 s2 a2) = 
    let 
        closed = Set.toList $ Set.intersection (Set.fromList closed1) (Set.fromList closed2)
    in
        if (t1 == t2) then 
            World2 t1 closed s1 s2 a1 a2
        else
            error $ "Times do not match!\n" ++ (show w1) ++ "\n" ++ (show w2)


nextStep2 :: ValveMap  -> PathMap -> WorldState2 -> H.HashSet WorldState2
nextStep2 m pm w@(World2 t closed s1 s2 a1 a2) = 
    let
        w1@(World t1 closed1 s1 a1) = smallWorld1 w
        w2@(World t2 closed2 s2 a2) = smallWorld2 w

{-
        closed1' = case target a2 of
            Nothing           -> closed1
            Just (Target x _) -> filter (not . (== x)) closed1

        closed2' = case target a1 of
            Nothing           -> closed2
            Just (Target x _) -> filter (not . (== x)) closed2
-}
        tg1 = case target a1 of 
            Just (Target x _) -> [x] 
            Nothing           -> []
        tg2 = case target a2 of 
            Just (Target x _) -> [x] 
            Nothing           -> []

        -- add an inbalance to cut the search tree in half?
        inbalance = vName $ head $ validValves m


        worlds1 = H.toList $! fixNoSteps w1 $ nextStep m pm (inbalance:tg2) (World t1 closed1 s1 a1)
        worlds2 = H.toList $! fixNoSteps w2 $ nextStep m pm tg1 (World t2 closed2 s2 a2)
    
        worlds' = do
            w1 <- worlds1
            w2 <- worlds2
            return (w1 `seq` w2 `seq` (largeWorld w1 w2))

{-
            if (step w1 == Wait) || (step w2 == Wait) 
                then 
                    trace ("Waiting at\n"  ++ (show w1) ++ "\n" ++ (show w2)) $ return (largeWorld w1 w2)
                else
-}
    in
        H.fromList $! worlds'

    where
        fixNoSteps :: WorldState -> H.HashSet WorldState -> H.HashSet WorldState
        fixNoSteps w steps = if H.null steps 
            then 
                H.singleton $ runStep pm w Wait
            else
                steps



closed2 :: WorldState2 -> ClosedValves
closed2 (World2 t closed s1 s2 a1 a2) = closed
    
cost2 :: ValveMap -> WorldState2 -> WorldState2 -> Int
cost2 m _ end = closedPressure m $ closed2 end

goal2 :: WorldState2 -> Bool
goal2 (World2 t closed s1 s2 a1 a2) = (t == maxMinute2) || null closed

step1 :: WorldState2 -> Step
step1 (World2 t closed s1 s2 a1 a2) = s1

step2 :: WorldState2 -> Step
step2 (World2 t closed s1 s2 a1 a2) = s2

time2 :: WorldState2 -> Int
time2 (World2 t closed s1 s2 a1 a2) = t

heuristic :: ValveMap -> ClosedValves -> Int
heuristic m closed = 
    let 
        rates = reverse $ sort $ map (vRate . (valveForString m)) closed
    in
        -- assume in the next step the largest valves will be closed
        sum $ drop 2 $ rates

maxMinute2 = 26

solutionValue2 :: ValveMap -> [WorldState2] -> Int
solutionValue2 m ws =
    sum $ map (uncurry (*)) $ solutionValue' m ws
  where 
    solutionValue' m ws = map (\w -> (maxMinute2 - time2 w, (openedPressure $ step1 w) + (openedPressure $ step2 w))) ws
    openedPressure (OpenValve vn) = vRate $ valveForString m vn
    openedPressure _              = 0


startWorld2 m =
    let 
        w = startWorld m 0
    in 
        largeWorld w w



part2 s =
    let
        Right m = regularParse parseFile s
        start = startWorld2 m
        pm = distanceMap m
        Just path = aStar (nextStep2 m pm) (cost2 m) ((heuristic m) . closed2 ) goal2 start
    in
        ((unlines $ map (\w -> (show $ time2 w) ++ " " ++ (show $ closed2 w) ++ "\t" ++ (show $ step1 w) ++ "\t" ++ (show $ step2 w)) path)
        ++ "\n" ++ (show $ solutionValue2 m path))




-- part2 = part1