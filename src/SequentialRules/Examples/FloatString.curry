{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=seqrules #-}

import Test.EasyCheck

-- Example: predicate to check for float strings

import Char(isDigit)

-- Is the argument a non-negative float string (without exponent)?
-- Our desired notation:
isNNFloat :: String -> Bool
isNNFloat (f1 ++ "." ++ f2) | (all isDigit f1 && all isDigit f2) = True
isNNFloat _ = False

main = map isNNFloat ["3.14","314"]

test1 = isNNFloat "3.14" -=- True
test2 = isNNFloat "314"  -=- False

