module MyLib (sumOverlaps) where

import Text.Parsec.String (Parser)
import Text.Parsec.Token (integer)
import Text.Parsec.Char (string, char, digit, oneOf, endOfLine)
import Text.Parsec.Combinator (many1, sepBy)
import Text.Parsec (parse, ParseError, eof)

import Data.Set (Set, intersection, elemAt, isSubsetOf)
import qualified Data.Set as Set

{-

OK, it's probably a bit over the top at the moment, but I'm going to start using parsec now, as recommended in https://wjwh.eu/posts/2022-11-30-haskell-aoc-tricks.html. It's probably good to gain experience with parsec when the problems are still relatively simple.

-}

testInput = "2-4,6-8\n\
\2-3,4-5\n\
\5-7,7-9\n\
\2-8,3-7\n\
\6-6,4-6\n\
\2-6,4-8"

-- https://github.com/JakeWheat/intro_to_parsing/blob/master/VerySimpleExpressions.lhs

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

num :: Parser Int
num = do
    n <- many1 digit
    return (read n)

data Range = Range Int Int
    deriving (Eq, Show)

data Pair = Pair [Range]
    deriving (Eq, Show)

range :: Parser Range
range = do
    min <- num
    string "-"
    max <- num
    return $ Range min max

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

overlap :: Pair -> Int
overlap (Pair rs) = 
    let [s1, s2] = map idSetFromRange rs
    in
        if (s1 `isSubsetOf` s2) || (s2 `isSubsetOf` s1) then 1 else 0

-- let x = regularParse pairList testInput
-- map overlap <$> x
-- sum <$> map overlap <$> x


sumOverlaps :: String -> Either ParseError Int
sumOverlaps s = sum <$> map overlap <$> regularParse pairList s
