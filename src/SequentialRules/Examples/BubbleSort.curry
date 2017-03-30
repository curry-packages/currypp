{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=seqrules #-}
-- Bubble sort formulation with default rules

import Test.EasyCheck
import SetFunctions

sort (xs++[x,y]++ys) | x>y = sort (xs++[y,x]++ys)
sort xs = xs

-- Compute only one value of sort:
sortOne xs = selectValue (set1 sort xs)

mainnd = sort [7,1,6,3,5,4,2]

-- Compute only first value:
main = sortOne [7,1,6,3,5,4,2]


bsort7 = sortOne [7,1,6,3,5,4,2] -=- [1..7]
