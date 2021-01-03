{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=contracts #-}
{-# OPTIONS_FRONTEND -Wnone #-}

import Test.Prop

-- A specification of sorting a list and an implementation based
-- on the quicksort algorithm

perm :: [a] -> [a]
perm []     = []
perm (x:xs) = insert (perm xs)
 where insert ys     = x:ys
       insert (y:ys) = y : insert ys

sorted :: Ord a => [a] -> Bool
sorted []       = True
sorted [_]      = True
sorted (x:y:ys) = x<=y && sorted (y:ys)

-- Trivial precondition, just for testing
sort'pre :: [Int] -> Bool
sort'pre xs = length xs >= 0

-- Postcondition: input and output lists should have the same length
sort'post :: [Int] -> [Int] -> Bool
sort'post xs ys = length xs == length ys

-- Specification of sort:
-- A list is a sorted result of an input if it is a permutation and sorted.
sort'spec :: [Int] -> [Int]
sort'spec x | y =:= perm x & sorted y = y  where y free

-- A buggy implementation of quicksort:
sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = sort (filter (<x) xs) ++ [x] ++ sort (filter (>x) xs)

input = [26,18,5,4,16,8,22,17]


-- Buggy implementation ok for different elements:
quickSortCorrect xs = allDifferent xs ==> always (sorted (sort xs))
 where
  allDifferent []     = True
  allDifferent (x:xs) = x `notElem` xs && allDifferent xs

-- In this example, the contract check should produce an error:
quickSortFailure = toError $ sort [1,2,1]
