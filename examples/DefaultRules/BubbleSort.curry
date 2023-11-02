{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=defaultrules #-}

import Test.Prop
import Control.Search.SetFunctions

-- Bubble sort formulation with default rule

sort :: [Int] -> [Int]
sort (xs++[x,y]++ys) | x>y = sort (xs++[y,x]++ys)
sort'default xs = xs

-- Compute only one value of sort:
sortOne xs = selectValue (set1 sort xs)

mainnd = sort [7,1,6,3,5,4,2]

-- Compute only first value:
main = sortOne [7,1,6,3,5,4,2]


bsort7 = sortOne [7,1,6,3,5,4,2] -=- [1..7]
