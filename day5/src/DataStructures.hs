module DataStructures where


type Slot = Maybe Char

type SlotRow = [Slot]

type NumberRow = [Int]

data Arrangement = Arrangement [SlotRow] NumberRow
    deriving (Eq, Show)

data Instruction = Move Int Int Int
    deriving (Eq, Show)

data Sheet = Sheet Arrangement [Instruction]
    deriving (Eq, Show)

type Stack = String