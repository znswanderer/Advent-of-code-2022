module MyLib (part1, part2) where


import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar, string, char, digit, oneOf, endOfLine, letter, spaces, alphaNum, noneOf, satisfy)
import Text.Parsec.Combinator (many1, sepBy1, endBy, lookAhead, sepBy)
import Text.Parsec (parse, ParseError, eof, (<|>), skipMany, manyTill, try, many)
import Data.Char (isSpace)

import Data.List (foldl', sort)
import GHC.Utils.Misc (nTimes)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import System.IO.Unsafe (unsafePerformIO)
-- Getting the text files for interactive ghci usage...
unsafeReadFile = unsafePerformIO . readFile 

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

num :: Parser Int
num = do
    n <- many1 digit
    return (read n)

type WorryLevel = Integer
type MonkeyNumber = Int
type Inspected = Int

data Monkey = Monkey MonkeyNumber [WorryLevel] Operation Test Inspected
    deriving (Eq, Show)

monkeyNumber :: Monkey -> MonkeyNumber
monkeyNumber (Monkey n _ _ _ _) = n

inspected :: Monkey -> Inspected
inspected (Monkey _ _ _ _ i) = i

allSpaceMonkeys = (many (spaces >> monkey))

type MonkeyCrew = IntMap Monkey
 
readMonkeyMap :: String -> MonkeyCrew
readMonkeyMap s = case regularParse allSpaceMonkeys s of
    Left err   -> error $ show err
    Right mnks -> IntMap.fromList $ zip (map monkeyNumber mnks) mnks


monkey :: Parser Monkey
monkey = do 
    string "Monkey "
    n <- num
    string ":"
    endOfLine

    spaces >> string "Starting items: "
    xs <- sepBy num (string ", ")
    endOfLine

    op <- operation
    endOfLine

    t <- test

    return $ Monkey n (map toInteger xs) op t 0


data Operation = Operation OpVal Char OpVal
    deriving (Eq, Show)

data OpVal = OldVal | NumVal Integer
    deriving (Eq, Show)

operation :: Parser Operation
operation = do
    spaces >> string "Operation: new = "
    x <- oldVal <|> numVal
    char ' '
    op <- anyChar
    char ' '
    y <-  oldVal <|> numVal
    return $ Operation x op y
  where
    oldVal = string "old" >> return OldVal
    numVal = num >>= (return . NumVal . toInteger)


data Test = Test Pred IfThen
    deriving (Eq, Show)

data Pred = DivBy Integer
    deriving (Eq, Show)

data IfThen = IfThen Command Command
    deriving (Eq, Show)

data Command = ThrowToMonkey Int
    deriving (Eq, Show)

test :: Parser Test
test = do
    spaces >> string "Test: "
    p <- predicate
    endOfLine
    ift <- ifThen
    return $ Test p ift 

ifThen :: Parser IfThen
ifThen = do
    spaces >> string "If true: "
    c1 <- command
    spaces >> string "If false: "
    c2 <- command
    return $ IfThen c1 c2


predicate :: Parser Pred
predicate = do
    string "divisible by "
    num >>= return . DivBy . toInteger

command :: Parser Command
command = do
    string "throw to monkey "
    num >>= return . ThrowToMonkey 


--- execution

monkeyBusiness :: MonkeyCrew -> MonkeyNumber -> MonkeyCrew
monkeyBusiness crew n =
    let
        Monkey mn wls op tst i = (IntMap.!) crew n
        i'                     = i + length wls
        wls'                   = map (runOperation op) wls
        wls''                  = map (\x -> x `div` 3) wls'
        acts                   = map (decideAction tst) wls''
        crew'                  = foldl' takeAction crew acts
    in
        IntMap.insert n (Monkey mn [] op tst i') crew'

        -- (b -> a -> b) -> b -> t a -> b

type Action = (WorryLevel, Command)

decideAction :: Test -> WorryLevel -> Action
decideAction (Test pr (IfThen c1 c2)) wl = if (testPred pr wl) then (wl, c1) else (wl, c2)

takeAction :: MonkeyCrew -> Action -> MonkeyCrew
takeAction crew (wl, ThrowToMonkey n) = 
    let
        m@(Monkey mn wls op tst i)  = (IntMap.!) crew n 
        m'                          = Monkey mn (wls ++ [wl]) op tst i
    in
        IntMap.insert n m' crew


val :: OpVal -> (WorryLevel -> WorryLevel)
val (OldVal)   = id
val (NumVal n) = const n

testPred :: Pred -> WorryLevel -> Bool
testPred (DivBy n) wl = (wl `mod` n) == 0

-- maybe State Monad??
runOperation :: Operation -> WorryLevel -> WorryLevel
runOperation (Operation x '+' y) n = (val x n) + (val y n)
runOperation (Operation x '*' y) n = (val x n) * (val y n)


runCrew :: MonkeyCrew -> MonkeyCrew
runCrew crew = (foldl' monkeyBusiness crew (IntMap.keys crew))

runCrewNTimes :: Int -> MonkeyCrew -> MonkeyCrew
runCrewNTimes n = nTimes n runCrew

inspectedAfterRounds :: Int -> MonkeyCrew -> [Inspected]
inspectedAfterRounds n crew = map inspected $ IntMap.elems $ runCrewNTimes n crew

------------------

part1 s = 
    let 
        (x:y:_) = reverse $ sort $ inspectedAfterRounds 20 $ readMonkeyMap s
    in
        x*y

-------------- PART 2

monkeyBusiness2 crew n =
    let
        Monkey mn wls op tst i = (IntMap.!) crew n
        -- for better performance (if needed) modValue should only calculated once
        modValue               = 
            foldl' (*) 1 $ map (\(Monkey _ _ _ (Test (DivBy n) _)  _) -> n) $ IntMap.elems crew
        i'                     = i + length wls
        wls'                   = map (runOperation op) wls
        wls''                  = map (\x -> x `mod` modValue) wls'
        acts                   = map (decideAction tst) wls''
        crew'                  = foldl' takeAction crew acts
    in
        IntMap.insert n (Monkey mn [] op tst i') crew'

runCrew2 :: MonkeyCrew -> MonkeyCrew
runCrew2 crew = (foldl' monkeyBusiness2 crew (IntMap.keys crew))

runCrewNTimes2 :: Int -> MonkeyCrew -> MonkeyCrew
runCrewNTimes2 n = nTimes n runCrew2

inspectedAfterRounds2 :: Int -> MonkeyCrew -> [Inspected]
inspectedAfterRounds2 n crew = map inspected $ IntMap.elems $ runCrewNTimes2 n crew

------------------

part2 s = 
    let 
        (x:y:_) = reverse $ sort $ inspectedAfterRounds2 10000 $ readMonkeyMap s
    in
        x*y