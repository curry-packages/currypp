{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
--- This program contains some examples for integrated code to format strings.
---
--- The format specification C specification for `printf` formatting.
--- This specification may be found at
--- <http://pubs.opengroup.org/onlinepubs/009695399/functions/fprintf.html>
------------------------------------------------------------------------------

import Format -- required in the pre-processed program

-- Format a string and an integer:
ex1 :: String -> Int -> IO ()
ex1 name age = putStrLn ``format "Hello %s. Age %i",name,age''

-- Format two integers:
ex2 :: Int -> Int -> IO ()
ex2 n1 n2 = putStrLn ``format "%+.5d and % 20.4i",n1,n2''

-- Format a charater:
ex3 :: Char -> IO ()
ex3 c = putStrLn ``format "This is a char: %c",c''

-- Format a string with a given width and maximal length:
ex4 :: String -> IO ()
ex4 s = putStrLn ``format "This is a string: %010.4s",s''

-- Format a float with a given width and precision:
ex5 :: Float -> IO ()
ex5 f = putStrLn ``format "This is a float: %+10.3f",f''

-- Format a float with an exponent:
ex6 :: Float -> IO ()
ex6 f = ``printf "This formats a float, too: % .4E\n",f''

-- Format the sum of two integers:
ex7 :: Int -> Int -> IO ()
ex7 n1 n2 = ``printf "The sum of %+.5d and %+5i is %+6i.\n",n1,n2,n1+n2''
