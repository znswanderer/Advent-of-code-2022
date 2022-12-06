module MyLib (findMarker, findMarker2) where

import Data.Set (Set)
import qualified Data.Set as Set

--import Debug.Trace


tests = [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
        , "bvwbjplbgvbhsrlpgdmjqwftvncz"
        , "nppdvjthqldpwncqszvftbrmjlhg"
        , "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
        , "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"]


varCheck codeLength pos msg = 
    let 
        s = Set.fromList $ take codeLength msg
    in 
        if length s == codeLength 
            then pos + codeLength
            else varCheck codeLength (pos + 1) (drop 1 msg)

findMarker :: String -> Int
findMarker = varCheck 4 0

findMarker2 :: String -> Int
findMarker2 = varCheck 14 0
