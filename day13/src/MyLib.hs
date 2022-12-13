module MyLib (part1, part2) where

import Data.List (sort, elemIndex)

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


data EMsg = I Int | L [EMsg]
    deriving (Eq, Show)

-- Unfortunately, the derivation of Ord does not yield the correct result,
-- so we have to construct the Ord instance ourselves


instance Ord EMsg where
    compare (I l)      (I r)      = compare l r
    compare (L [])     (L [])     = EQ
    compare (L [])     (L _)      = LT
    compare (L _)      (L [])     = GT
    compare (L (l:ls)) (L (r:rs)) = 
        let pos1 = compare l r
        in if pos1 == EQ then compare (L ls) (L rs) else pos1
    compare (I l)    r@(L _)      = compare (L [I l]) r
    compare l@(L _)    (I r)      = compare l         (L [I r])

eVal :: Parser EMsg
eVal = eInt <|> eList
    
eInt :: Parser EMsg
eInt = do
    n <- num
    return $ I n

eList :: Parser EMsg
eList = do
    char '['
    vals <- sepBy eVal (char ',')
    char ']'
    return $ L vals

readL :: Parser [EMsg]
readL = sepBy eList spaces

runList :: [EMsg] -> [Ordering]
runList [] = []
runList (l:r:xs) = (compare l r):runList xs


allValidIndices :: [EMsg] -> [Int]
allValidIndices xs = map fst $ filter (\(n, x) -> x == LT) $ zip [1..] $ runList xs

part1 :: [Char] -> Int
part1 s = 
    let Right xs = regularParse readL s
    in sum $ allValidIndices xs

part2 :: [Char] -> Int
part2 s = 
    let 
        Right xs = regularParse readL s
        Right ys = regularParse readL "[[2]]\n[[6]]"
        xs' = xs ++ ys
        xs'' = sort xs'
        [Just a, Just b] = map ((flip elemIndex) xs'') ys
    in 
        (a+1) * (b+1)