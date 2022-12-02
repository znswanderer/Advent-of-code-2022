module MyLib (someFunc, getScore, getScorePart2) where

data Hand = Rock | Paper | Scissors
    deriving (Eq, Show)

data Match = Match Hand Hand
    deriving (Eq, Show)

data MatchResult = Win | Loose | Tie
    deriving (Eq, Show)

data Target = Target Hand MatchResult
    deriving (Eq, Show)


someFunc :: IO ()
someFunc = do 
    -- putStrLn testInput
    let matches = readMatches testInput
    putStrLn $ show $ matches
    putStrLn $ show $ map matchResult $ matches
    let scores = map matchScore matches
    putStrLn $ show scores
    putStrLn $ show $ sum scores

getScore :: String -> Int
getScore input = 
    sum scores
  where
    matches = readMatches input
    scores = map matchScore matches

getScorePart2 :: String -> Int
getScorePart2 input = 
    sum scores
  where
    scores = map matchScore matches
    matches = map target2Match targets
    targets = readTargets input


-- Maybe better with a Map?
getHand :: Char -> Hand
getHand 'A' = Rock
getHand 'B' = Paper
getHand 'C' = Scissors
getHand 'X' = Rock
getHand 'Y' = Paper
getHand 'Z' = Scissors

handScore :: Hand -> Int
handScore Rock     = 1
handScore Paper    = 2
handScore Scissors = 3

score2Hand :: Int -> Hand
score2Hand 1 = Rock
score2Hand 2 = Paper
score2Hand 3 = Scissors


{-
Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock.

Scissors defeats Paper: 3 - 2 = 1
Paper defeats Rock: 2 - 1 = 1
Rock defeats Scissors: 1 - 3 = -2
Paper vs Scissors: 2 - 3 = -1
Rock vs Paper: 1 - 2 = -1
Scissors vs Rock: 3 - 1 = 2

This is like a distance vector in periodic boundary conditions.

x_a - x_b

1----2----3(---4)
R    P    S    R

P beats R
R<---P

S beats P
     P<---S

R beats S
          S<---R

python:
>>> (1 - 3) % 3
1
>>> (2 - 1) % 3
1
>>> (3 - 2) % 3
1
>>> (1 - 2) % 3
2
>>> (2 - 3) % 3
2
>>> (3 - 1) % 3
2

-}


resultScore :: MatchResult -> Int
resultScore Win   = 6
resultScore Tie   = 3
resultScore Loose = 0


matchScore :: Match -> Int
matchScore m@(Match _ myHand) = 
    (resultScore result) + (handScore myHand)
  where
    result = matchResult m

matchResult :: Match -> MatchResult
matchResult (Match h1 h2) =
    match h1 h2
  where
    match :: Hand -> Hand -> MatchResult
    match other me = compareScores $ (handScore other) - (handScore me)
    compareScores :: Int -> MatchResult
    compareScores n = 
        case (n `mod` 3) of
            1 -> Loose
            2 -> Win
            0 -> Tie

readMatches :: String -> [Match]
readMatches input = 
    map getMatch $ lines input
  where
    otherHand line = getHand (line !! 0)
    myHand    line = getHand (line !! 2)
    getMatch  line = Match (otherHand line) (myHand line)

-- PART 2

readTargets :: String -> [Target]
readTargets input =
    map getTarget $ lines input
  where
    otherHand line = getHand (line !! 0)
    result    line = case (line !! 2) of
                        'X' -> Loose
                        'Y' -> Tie
                        'Z' -> Win
    getTarget line = Target (otherHand line) (result line)

target2Match :: Target -> Match
target2Match (Target otherHand matchResult) =
    Match otherHand (handForResult matchResult otherHand)
  where
    handForResult :: MatchResult -> Hand -> Hand
    handForResult Tie   h = h
    handForResult Loose h = score2Hand $ (((handScore h) - 2) `mod` 3) + 1
    handForResult Win   h = score2Hand $ ((handScore h) `mod` 3) + 1


testInput = "A Y\n\
\B X\n\
\C Z"

