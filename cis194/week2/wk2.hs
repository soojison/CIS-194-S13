-- Week 2 of CIS 194

-- Enumeration types
data Thing = Shoe
            | Ship
            | SealingWax
            | Cabbage
            | King
    deriving Show
-- This declares a new type called Thing with 5 data constructors

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

-- functions on Things by pattern-matching
isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False

-- we could also make the def. of isSmall shorter
-- because function clauses are tried in order from top to bottom
isSmall2 :: Thing -> Bool
isSmall2 Ship = False
isSmall2 King = False
isSmall2 _    = True

-- Beyond enumerations
