module MyLib (someFunc, getScore) where

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

data Hand = Rock | Paper | Scissors
    deriving (Eq, Show)

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

-- Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock.

-- Scissors defeats Paper: 3 - 2 = 1
-- Paper defeats Rock: 2 - 1 = 1
-- Rock defeats Scissors: 1 - 3 = -2
-- Paper vs Scissors: 2 - 3 = -1
-- Rock vs Paper: 1 - 2 = -1
-- Scissors vs Rock: 3 - 1 = 2

data Match = Match Hand Hand
    deriving (Eq, Show)

data MatchResult = Win | Loose | Tie
    deriving (Eq, Show)

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
    compareScores 1    = Loose
    compareScores (-2) = Loose
    compareScores (-1) = Win
    compareScores 2    = Win
    compareScores 0    = Tie

readMatches :: String -> [Match]
readMatches input = 
    map getMatch $ lines input
  where
    otherHand line = getHand (line !! 0)
    myHand    line = getHand (line !! 2)
    getMatch  line = Match (otherHand line) (myHand line)

testInput = "A Y\n\
\B X\n\
\C Z"

