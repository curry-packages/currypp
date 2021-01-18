{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=defaultrules --optF=contracts #-}

import Test.Prop

-- Bubble sort formulation with default rule as deterministic operation:
sort :: [Int] ->DET [Int]
sort (xs ++ [x,y] ++ ys) | x>y = sort (xs ++ [y,x] ++ ys)
sort'default xs = xs

-- Precondition: we don't like to sort empty lists...
sort'pre :: [Int] -> Bool
sort'pre xs = length xs > 0

-- Postcondition: input and output lists should have the same length
sort'post :: [Int] -> [Int] -> Bool
sort'post xs ys = length xs == length ys

sort7 = sort [7,1,6,3,5,4,2] -=- [1..7]

sortEmpty = toError (sort [])


