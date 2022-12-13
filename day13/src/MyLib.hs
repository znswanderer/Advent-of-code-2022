module MyLib (part1, part2) where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (anyChar, string, char, digit, oneOf, endOfLine, letter, spaces, alphaNum, noneOf, satisfy)
import Text.Parsec.Combinator (many1, sepBy1, endBy, lookAhead, sepBy)
import Text.Parsec (parse, ParseError, eof, (<|>), skipMany, manyTill, try, many)

import System.IO.Unsafe (unsafePerformIO)
-- Getting the text files for interactive ghci usage...
unsafeReadFile = unsafePerformIO . readFile 

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

num :: Parser Int
num = do
    n <- many1 digit
    return (read n)


data EMsg = EInt Int | EList [EMsg]
    deriving (Eq, Show)

x = [EInt 1, EInt 2, EList [EInt 2, EInt 3]]

elfie :: EMsg -> EMsg -> Ordering
elfie (EInt l)        (EInt r)       = compare l r
elfie (EList [])      (EList [])     = EQ
elfie (EList [])      (EList _)      = LT
elfie (EList _)       (EList [])     = GT
elfie (EList (l:ls))  (EList (r:rs)) = 
    let pos1 = elfie l r
    in if pos1 == EQ then elfie (EList ls) (EList rs) else pos1
elfie (EInt l)        r@(EList _)    = elfie (EList [EInt l]) r
elfie l@(EList _)     (EInt r)       = elfie l (EList [EInt r])

eVal :: Parser EMsg
eVal = eInt <|> eList
    
eInt :: Parser EMsg
eInt = do
    n <- num
    return $ EInt n

eList :: Parser EMsg
eList = do
    char '['
    vals <- sepBy eVal (char ',')
    char ']'
    return $ EList vals

eCompare :: String -> String -> Ordering
eCompare l r = 
    let 
        Right l' = regularParse eList l
        Right r' = regularParse eList r
    in
        elfie l' r'

readEList :: Parser [EMsg]
readEList = sepBy eList spaces

runList :: [EMsg] -> [Ordering]
runList [] = []
runList (l:r:xs) = (elfie l r):runList xs


allValidIndices :: [EMsg] -> [Int]
allValidIndices xs = map fst $ filter (\(n, x) -> x == LT) $ zip [1..] $ runList xs

part1 s = 
    let Right xs = regularParse readEList s
    in sum $ allValidIndices xs

part2 = const "part2"