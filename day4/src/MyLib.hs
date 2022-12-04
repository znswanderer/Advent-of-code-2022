module MyLib (sumFullOverlaps, sumPartialOverlaps) where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, char, digit, oneOf, endOfLine)
import Text.Parsec.Combinator (many1, sepBy)
import Text.Parsec (parse, ParseError, eof)

import Data.Set (Set, intersection, isSubsetOf)
import qualified Data.Set as Set

import System.IO.Unsafe (unsafePerformIO)

{-

OK, it's probably a bit over the top at the moment, but I'm going to start 
using parsec now, as recommended in
https://wjwh.eu/posts/2022-11-30-haskell-aoc-tricks.html. It's probably 
good to gain experience with parsec when the problems are still relatively
simple.

-}

-- Getting the text files for interactive ghci usage...
unsafeReadFile = unsafePerformIO . readFile 


-- https://github.com/JakeWheat/intro_to_parsing/blob/master/VerySimpleExpressions.lhs

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

num :: Parser Int
num = do
    n <- many1 digit
    return (read n)

data Range = Range Int Int
    deriving (Eq, Show)

range :: Parser Range
range = do
    min <- num
    string "-"
    max <- num
    return $ Range min max

data Pair = Pair [Range]
    deriving (Eq, Show)

pair :: Parser Pair
pair = do
    first <- range
    string ","
    second <- range
    return $ Pair [first, second]

pairList :: Parser [Pair]
pairList = pair `sepBy` endOfLine >>= \ps -> eof >> return ps


idSetFromRange :: Range -> Set.Set Int
idSetFromRange (Range from to) = Set.fromList $ enumFromTo from to

fullOverlap :: Pair -> Int
fullOverlap (Pair rs) = 
    let [s1, s2] = map idSetFromRange rs
    in
        if (s1 `isSubsetOf` s2) || (s2 `isSubsetOf` s1) then 1 else 0

partialOverlap :: Pair -> Int
partialOverlap (Pair rs) = 
    let [s1, s2] = map idSetFromRange rs
    in if null (s1 `intersection` s2) then 0 else 1


sumFullOverlaps :: String -> Either ParseError Int
sumFullOverlaps s = sum <$> map fullOverlap <$> regularParse pairList s

sumPartialOverlaps :: String -> Either ParseError Int
sumPartialOverlaps s = sum <$> map partialOverlap <$> regularParse pairList s
