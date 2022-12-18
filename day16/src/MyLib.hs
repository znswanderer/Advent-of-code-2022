module MyLib (part1) where

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




---------------------------------------------

data Step = Wait | MoveTo ValveName | OpenValve ValveName
    deriving (Eq, Show, Ord)



type Time = Int
type ClosedValves = [ValveName]
type Position = ValveName

data WorldState = World Time ClosedValves Position Step
    deriving (Eq, Show, Ord)

instance Hashable WorldState where
    hashWithSalt n = (hashWithSalt n). show

nextStep :: ValveMap -> WorldState -> H.HashSet WorldState
nextStep m w@(World t closed pos s) = 
    let 
        thisValve = valveForString m pos
        openValve = if (vRate thisValve > 0) && (pos `elem` closed) then [OpenValve pos] else []
        moves = map MoveTo $ vExits thisValve
        steps = [Wait] ++ openValve ++ moves
    in
        H.fromList $ map (\s -> runStep w s) steps

runStep :: WorldState -> Step -> WorldState
runStep (World t closed pos _) Wait            = World (t+1) closed pos Wait
runStep (World t closed _ _)   (MoveTo newpos) = World (t+1) closed newpos (MoveTo newpos)
runStep (World t closed pos _) (OpenValve v)   = World (t+1) (filter (not . (==v)) closed) pos (OpenValve v)

time :: WorldState -> Int
time (World t _ _ _) = t

closed :: WorldState -> ClosedValves
closed (World _ closed _ _) = closed

step :: WorldState -> Step
step (World t closed _ s) = s

closedPressure :: ValveMap -> ClosedValves -> Int
closedPressure m closed = sum $ (map (vRate . (valveForString m)) closed)

startWorld m = World 0 (map vName $ validValves m) startValveName (MoveTo startValveName)

cost :: ValveMap -> WorldState -> WorldState -> Int
cost m _ end = closedPressure m $ closed end

goal :: WorldState -> Bool
goal (World t closed _ _) = (t == maxMinute) || (null closed)

solutionValue :: ValveMap -> [WorldState] -> Int
solutionValue m ws =
    sum $ map (uncurry (*)) $ solutionValue' m ws
  where 
    solutionValue' m ws = map (\w -> (maxMinute - time w, openedPressure $ step w)) ws
    openedPressure (OpenValve vn) = vRate $ valveForString m vn
    openedPressure _              = 0



valveForString :: ValveMap -> ValveName -> Valve
valveForString m s = fromJust $ Map.lookup s m

startValveName = "AA"

maxMinute = 30 :: Int



validValves :: ValveMap -> [Valve]
validValves m = filter (\v -> vRate v > 0) (Map.elems m)


part1 s =
    let
        Right m = regularParse parseFile s
        start = startWorld m
        Just path = aStar (nextStep m) (cost m) (\x -> 0) goal start
    in
        show $ solutionValue m path


