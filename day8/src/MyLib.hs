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


{-
    eg. runner [2,5,5,1,2] = [True,True,True,False,True]
-}
runner xs = True:(runner' (take 1 xs) (drop 1 xs))

 
runner' pre (curr:[]) = [True]
runner' pre (cur:aft) = vis:(runner' (pre ++ [cur]) aft)
  where
    comp = map (\x -> if x >= cur then 1 else 0) 
    pre' = comp pre
    aft' = comp aft
    vis  = (sum pre' == 0) || (sum aft' == 0)


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