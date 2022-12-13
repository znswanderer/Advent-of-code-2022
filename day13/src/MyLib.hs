 {-# LANGUAGE FlexibleInstances #-}
 {-# LANGUAGE MultiParamTypeClasses #-}
 
module MyLib (part1, part2) where



{-
class ElfComp a  b where
    ecomp :: a -> b -> Bool

instance (ElfComp a b) => (ElfComp [a] [b]) where
    ecomp xs ys =
        if (length xs) > (length ys) 
            then False
            else and $ map (uncurry ecomp) (zip xs ys)

instance ElfComp Integer Integer where
    ecomp a b = a <= b

instance (ElfComp a b) => (ElfComp a [b]) where
    ecomp x ys = ecomp [x] ys

-}

class ElfComb a where
    ---

ecompInt :: Int -> Int -> Bool
ecompInt a b = a <= b

ecompIntList :: [Int] -> 


part1 = const "part1"

part2 = const "part2"