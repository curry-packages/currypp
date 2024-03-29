{-# OPTIONS_FRONTEND -F --pgmF=currypp #-}
{-# OPTIONS_FRONTEND -Wnone #-}

-- Example for using integrated code, default rules, and contracts in one
-- module

import Control.Search.SetFunctions
import Data.Format
import Test.Contract
import Test.Prop

showInt :: Int -> String
showInt i =  ``format "%+.3d",i''

-- Bubble sort formulation with default rule as deterministic operation:
sort :: (Data a, Ord a, Show a) => [a] ->DET [a]
sort (xs ++ [x,y] ++ ys) | x>y = sort (xs ++ [y,x] ++ ys)
sort'default xs = xs

-- Precondition: we don't like to sort empty lists...
sort'pre :: (Data a, Ord a, Show a) => [a] -> Bool
sort'pre xs = length xs > 0

-- Postcondition: input and output lists should have the same length
sort'post :: (Data a, Ord a, Show a) => [a] -> [a] -> Bool
sort'post xs ys = length xs == length ys

sort7 =
  sort (map showInt [7,1,6,3,5,4,2]) -=- map (\d -> "+00" ++ show d) [1..7]

sortEmpty = toError (sort ([] :: [Int]))
