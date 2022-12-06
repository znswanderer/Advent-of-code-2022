module MyLib (findMarker) where

import Data.Set (Set)
import qualified Data.Set as Set

--import Debug.Trace


tests = [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
        , "bvwbjplbgvbhsrlpgdmjqwftvncz"
        , "nppdvjthqldpwncqszvftbrmjlhg"
        , "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
        , "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]

cursor = zip [1..]

check l@((_, a):(_, b):(_, c):(n, d):_) = 
    let s = Set.fromList [a,b,c,d]
    in
--        if length (trace ((show n) ++ " " ++ (show s)) s) == 4 then n else check (drop 1 l)
        if length s == 4 then n else check (drop 1 l)

examples = map check $ map cursor tests

findMarker :: String -> Int
findMarker = check . cursor