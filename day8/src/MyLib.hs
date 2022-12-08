module MyLib (part1, part2) where

import Data.List (transpose)

-- import GHC.Arr (Array, array)

--import Data.Array


{-

I wonder, if one can solve this by treating the lines
as numbers and then using maybe logarithm base 10
or polynomials

-}

type Grid a = [[a]]

a = [[1, 2], [3, 4]]

row :: Grid a -> Int -> [a]
row g n = g !! n

column :: Grid a -> Int -> [a]
column g n = map (\x -> x !! n) g

pos :: Grid a -> Int -> Int -> a
pos g x y = (column g x) !! y

dimension :: Grid a -> (Int, Int)
dimension g = ((length $ row g 0), (length $ column g 0))

test = "30373\n\
\25512\n\
\65332\n\
\33549\n\
\35390"

single :: [a] -> [[a]]
single (x:xs) = [x]:single xs
single []     = []

readLine l = (map read (single l)) :: [Int]

readGrid = map readLine . lines

testGrid = map readLine $ lines test

--check :: Grid Int -> Int -> Int -> Bool
check g x y = 
    let r = row g y
        preCol = take (x) r
        aftCol = drop (x+1) r
        c = column g x
        preRow = take (y) c
        aftRow = drop (y+1) c
    in [preCol, aftCol, preRow, aftRow]


r0 = [3, 0, 3, 7, 3]

-- splitAt :: Int -> [a] -> ([a], [a]) 

{-
n = 2
cur = r0 !! n
(pre, aft) = splitAt n r0
comp = map (\x -> if x > cur then 1 else 0) 
pre' = comp pre
aft' = comp aft
-}

{-
    "runner xs" will calculate for each position in the list xs
    how many of the Int values of a given position are larger
    than the current position.

    eg. runner [2,5,5,1,2] = [0,0,0,4,0]

    The first and the last index a spared from comparison.
    The value "4" for the list value "1" is because 4 entries
    are larger than 1.
-}
runner xs = True:(runner' (take 1 xs) (drop 1 xs))

 
runner' pre (curr:[]) = [True]
runner' pre (cur:aft) = vis:(runner' (pre ++ [cur]) aft)
  where
    comp = map (\x -> if x >= cur then 1 else 0) 
    pre' = comp pre
    aft' = comp aft
    vis  = (sum pre' == 0) || (sum aft' == 0)

-- Is a given position visible?
--visible xs = map (\x -> if x > 0 then 0 else 1) $ runner xs

visibleByRows :: Grid Int -> Grid Bool
visibleByRows = map runner


visibleByColumns :: Grid Int -> Grid Bool
visibleByColumns = transpose . map runner . transpose




onTwo _ [] []         = []
onTwo f (x:xs) (y:ys) = (f x y):(onTwo f xs ys)

--map (\(x,y) -> onTwo (+) x y) $ zip testGrid testGrid

calcOnGrids :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
calcOnGrids f ga gb = map (\(x,y) -> onTwo f x y) $ zip ga gb


visibleByBoth :: Grid Int -> Grid Bool
visibleByBoth g =  
        calcOnGrids (||) (visibleByRows g) (visibleByColumns g)

{-

visibleOneWay :: Grid Int -> Grid Int
visibleOneWay g =  
    let 
        f x y = if x + y > 0 then 1 else 0
    in
        calcOnGrids f (visibleByRows g) (visibleByColumns g)

{-
visibleByBoth :: Grid Int -> Grid Int
visibleByBoth g = 
    let 
        f x y = if x + y == 2 then 1 else 0
    in
        map (\(x,y) -> onTwo f x y) $ zip (visibleByRows g) (visibleByColumns g)
-}

-}

-- Can be done better
inner xs = 
    let l = length xs
    in take (l - 2) $ drop 1 xs

innerGrid :: Grid a -> Grid a
innerGrid g = map inner $ inner g

innerCount = length . filter id . concat . innerGrid . visibleByBoth


part1 s = 
    let g          = readGrid s
        l          = length $ row g 0
        outerCount = 2*l + 2*(l-2)
    in outerCount + (innerCount g)

 
part2 = part1