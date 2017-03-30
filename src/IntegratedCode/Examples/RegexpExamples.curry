{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
--- This program contains some examples for integrated code to support
--- regular expression matching.
--- The syntax of regular expressions is similar to
--- POSIX extended regular expressions
------------------------------------------------------------------------------

import RegExp -- required in the pre-processed program

check1 :: Bool
check1 = "abc" ``regex abc''

check2 :: Bool
check2 = "abaaaaaaaaaaaaac" ``regex aba*c''

check3 :: String -> Bool
check3 s = s ``regex (a|(bc*))+''

check4 :: String -> Bool
check4 s = s ``regex [:alpha:]''

check5 :: String -> Bool
check5 s = s ``regex [a-z]+''

-- Examples with parameterized regular expressions:

pregexp1 :: [a] -> a -> a -> Bool
pregexp1 s v1 v2 = s ``regex [<v1>-<v2>]*''

pregexp2 :: [a] -> a -> a -> Bool
pregexp2 s v1 v2 = s ``regex (<v1>|<v2>)*''

-- A regular expression containing a complex Curry expression:
check6 :: Bool
check6 = "a" ``regex <((\x -\> x) 'a')>'' 

