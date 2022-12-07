module Parsers where

import DataTypes

import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, char, digit, oneOf, endOfLine, letter, spaces, alphaNum, noneOf, satisfy)
import Text.Parsec.Combinator (many1, sepBy1, endBy, lookAhead, sepBy)
import Text.Parsec (parse, ParseError, eof, (<|>), skipMany, manyTill, try, many)
import Data.Char (isSpace)


data DirContent = DirToken DirName | FileToken FileSize FileName
    deriving (Eq, Show)

data Interaction = CD DirName | LS [DirContent]
    deriving (Eq, Show)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p ""

session :: Parser [Interaction]
session = many interaction 

interaction :: Parser Interaction
interaction = do
    string "$ "
    (try cdCommand) <|> lsCommand

cdCommand :: Parser Interaction
cdCommand = do
    string "cd "
    s <- many1 (satisfy (not . isSpace))
    endOfLine
    return $ CD s

lsCommand :: Parser Interaction
lsCommand = do 
    string "ls" >> endOfLine
    contents <- dirContent `endBy` endOfLine
    return $ LS contents

dirContent :: Parser DirContent
dirContent = (try dir) <|> file
  where
    dir = do
        string "dir " 
        name <- many1 alphaNum
        return $ DirToken name
    file = do 
        sz <- many1 digit
        char ' '
        fn <- fileName
        return $ FileToken (read sz) fn
        
fileName :: Parser FileName
fileName = many1 (alphaNum <|> char '.')


{-
fileName :: Parser FileName
fileName = do
    (try withExtension) <|> noExtension
  where
    withExtension = do
        s <- many1 alphaNum
        char '.'
        ext <- many1 alphaNum
        return $ FileName s ext
    noExtension = do
        s <- many1 alphaNum
        return $ FileName s ""


-}


