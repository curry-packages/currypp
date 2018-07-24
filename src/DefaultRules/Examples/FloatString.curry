{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

import Test.EasyCheck

-- Example: predicate to check for float strings

import Data.Char (isDigit)

-- Is the argument a non-negative float string (without exponent)?
isNNFloat :: String -> Bool
isNNFloat (f1 ++ "." ++ f2) | (all isDigit f1 && all isDigit f2) = True
isNNFloat'default _ = False

main = map isNNFloat ["3.14","314"]

test1 = isNNFloat "3.14" -=- True
test2 = isNNFloat "314"  -=- False
