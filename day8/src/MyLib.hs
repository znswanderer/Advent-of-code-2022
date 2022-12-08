module MyLib (part1, part2) where

import Data.List (transpose, sort)

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
testGrid = readGrid test


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


-- maybe zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] ?
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

{-
    Part 1 can also be solved by looking at the forest from four sides as a
    photo and then look at how many trees are visible, i.e.

    photo [3, 0, 3, 7, 3] = [3, 7]

    And then continue with reverse and transpose...
-}

photo (x:xs) = x:photo' x xs
    where
        photo' _    []     = []
        photo' maxX (x:xs) = if maxX >= x then photo' maxX xs else x:photo' x xs
 


 -- ---------------- PART 2 -------------------



{-
    leftView [2,5,5,1,2] = [0,1,1,1,2]
    rightView [2,5,5,1,2] = [1,1,2,1,0]

-}


leftView (x:xs) = reverse $ leftView' [0] [] xs
  where
    leftView' acc _ []          = acc
    leftView' acc pre (cur:aft) = 
        let
            acc' = (length (takeWhile (< cur) pre) + 1):acc
            pre' = (cur:pre)
        in 
            leftView' acc' pre' aft
    
rightView xs = reverse $ leftView $ reverse xs

horizScore g = calcOnGrids (*) (map leftView g) (map rightView g)

vertScore g = 
    let 
        g' = transpose g
    in 
        transpose $ calcOnGrids (*) (map leftView g') (map rightView g')

scenicScore g = calcOnGrids (*) (horizScore g) (vertScore g)

highestScenicScore g = head $ reverse $ sort $ concat $ scenicScore g

part2 = highestScenicScore . readGrid