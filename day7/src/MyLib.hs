module MyLib (part1, part2) where

import Parsers
import DataTypes

import Data.List (intercalate, sort)

import Debug.Trace

import System.IO.Unsafe (unsafePerformIO)
-- Getting the text files for interactive ghci usage...
unsafeReadFile = unsafePerformIO . readFile 

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- http://learnyouahaskell.com/zippers

x -: f = f x

type Name = String
type DataSize = Int

data FSItem = File Name DataSize | Folder Name [FSItem] deriving (Eq, Show)

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Eq, Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

prettyPrint :: FSItem -> String
prettyPrint = prettyPrint' 0 
    where
        prettyPrint' n  item = (replicate n ' ') ++ " - " ++ (prettyPrint'' n item)
        prettyPrint'' n (File name size) = name ++ " (file, size=" ++ (show size) ++ ")"
        prettyPrint'' n (Folder name ls) = 
            name ++ " (dir)" ++ "\n" ++ (intercalate "\n" $ map (prettyPrint' (n + 1)) ls)

 

  
{-
Example Tree:
(I changed d and c for having an inner dir "c" in root)

- / (dir)
  - a (dir)
    - e (dir)
  - b.txt (file, size=14848514)
  - c (dir)
  - d.dat (file, size=8504156)
-}

exRoot = (Folder "/" [Folder "a" [Folder "e" []],File "b" 14848514,Folder "c" [],File "d" 8504156],[])

-- If we are at "a" this is the zipper: (with empty subdirs)
exZipper = ((Folder "c" []), [FSCrumb 
                                "/" 
                                [Folder "a" [Folder "e" []], File "b" 14848514] 
                                [File "d" 8504156]])

-- If we are at "a/e" this is the zipper: (with empty subdirs)
exZipper2 = ((Folder "e" []), 
                [
                    FSCrumb "a" [] [],
                    FSCrumb "/" [] [File "b" 14848514, Folder "c" [], File "d" 8504156]])

-- If we are at "a" this is the zipper: (with empty subdirs)
exZipper3 = ((Folder "a" [Folder "e" []]), 
                [FSCrumb "/" [] [File "b" 14848514, Folder "c" [], File "d" 8504156]])


fsCd :: Name -> FSZipper -> FSZipper
fsCd subDirName ((Folder name items), cs) =
    let (ls, item:rs) = break isDirName items
    in
        (item, (FSCrumb name ls rs):cs)
  where
    isDirName (Folder s _) = s == subDirName
    isDirName _            = False

fsInsert :: [FSItem] -> FSZipper -> FSZipper 
fsInsert items (Folder name _, cs) = (Folder name items, cs)

empty = ((Folder "/" []), []) :: FSZipper

example = empty 
    -: fsInsert [Folder "a" [], File "b.txt" 14848514, File "c.dat" 8504156, Folder "d" []]
    -: fsCd "a"
    -: fsInsert [Folder "e" [], File "f" 29116, File "g" 2557, File "h.lst" 62596]
    -: fsCd "e"
    -: fsInsert [File "i" 584]
    -: fsUp
    -: fsUp
    -: fsCd "d"
    -: fsInsert [File "j" 4060174, File "d.log" 8033020, File "d.ext" 5626152, File "k" 7214296]

topMost :: FSZipper -> FSZipper
topMost r@(item, []) = r
topMost z = topMost $ fsUp z

root :: FSZipper -> FSItem
root fs = 
    let (item, []) = topMost fs
    in 
        item

applyInteraction :: Interaction -> FSZipper -> FSZipper
applyInteraction (CD "/")  = topMost
applyInteraction (CD "..") = fsUp
applyInteraction (CD name) = fsCd name
applyInteraction (LS ls)   = fsInsert $ map convertDirContent ls

convertDirContent :: DirContent -> FSItem
convertDirContent (DirToken name)     = Folder name []
convertDirContent (FileToken sz name) = File name sz

-- Debug Build
debugBuild z [] = z
debugBuild z (step:steps) = (trace (show step)) $ debugBuild (applyInteraction step z) steps

build :: String -> FSItem
build s = case regularParse session s of
    Right steps -> root $ foldl (flip applyInteraction) empty steps
    Left  err   -> error $ show err
    
dataSize :: FSItem -> DataSize
dataSize (File _ size) = size
dataSize (Folder name ls) = sum $ map dataSize ls

allFolders :: (FSItem -> a) -> FSItem ->  [a]
allFolders f (File _ _)          = []
allFolders f fd@(Folder name ls) = (f fd):(concat $ map (allFolders f) ls)

part1 :: String -> DataSize
part1 s = sum $ filter (<= 100000) $ allFolders dataSize $ build s

part2 :: String -> DataSize
part2 s = 
    let fs = build s
        sizes = allFolders dataSize fs
        usedSize = head $ sizes
        freeSize = 70000000 - usedSize
        missing = 30000000 - freeSize
    in head $ sort $ filter (>= missing) sizes
    




--data SizedFolder = SizedFolder Name [SizedFolder] DataSize deriving (Show)

--getSizes :: FSItem -> SizedFolder
--getSizes (Folder name ls) = 