module Parsers where

import DataStructures

import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, char, digit, oneOf, endOfLine, letter, spaces)
import Text.Parsec.Combinator (many1, sepBy1, endBy1, lookAhead, sepBy)
import Text.Parsec (parse, ParseError, eof, (<|>), skipMany, manyTill, try)


regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

slot :: Parser Slot
slot = do
    filled <|> empty
  where
    filled = do 
        string "["
        c <- letter
        string "]"
        return $ Just c
    empty = do 
        string "   "
        return Nothing


slotRow :: Parser SlotRow
slotRow = slot `sepBy1` (char ' ')



num :: Parser Int
num = do
    n <- many1 digit
    return (read n)

numberRow :: Parser [Int]
numberRow = colNum `sepBy1` (char ' ')
    where
        colNum = do
            char ' '
            n <- num
            char ' '
            return n

arrangement :: Parser Arrangement
arrangement = do
    rs <- ((try slotRow) `endBy1` endOfLine)
    nr <- withEOL numberRow
    return $ Arrangement rs nr


withEOL :: Parser a -> Parser a
withEOL p = do
    res <- p
    endOfLine
    return res


instruction :: Parser Instruction
instruction = do
    string "move" >> spaces
    n <- num
    spaces >> string "from" >> spaces
    from <- num
    spaces >> string "to" >> spaces
    to <- num
    return $ Move n from to


sheet :: Parser Sheet
sheet = do
    arr <- arrangement
    endOfLine
    ins <- ((try instruction) `sepBy1` endOfLine)
    return $ Sheet arr ins