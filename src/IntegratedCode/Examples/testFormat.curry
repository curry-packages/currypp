{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
--- This program contains some examples for integrated code to support
--- string formatting.
---
--- The format specification follows the C specification for `printf`
--- formatting whch may be found at
--- <http://pubs.opengroup.org/onlinepubs/009695399/functions/fprintf.html>
------------------------------------------------------------------------------

import Format -- required in the pre-processed program
import Test.EasyCheck

-- Format a string and an integer:
ex1 :: String -> Int -> String
ex1 name age = ``format "Hello %s, you are %i years old.",name,age''

testEx1 = (ex1 "Mike" 42) -=- "Hello Mike, you are 42 years old."

-- Format two integers:
ex2 :: Int -> Int -> String
ex2 n1 n2 = ``format "%+.5d and % 10.4i",n1,n2''

testEx2 = (ex2 12345 34) -=- "+12345 and       0034"

testInt5            = ``format "%.5i",42''    -=- "00042"

testIntSigned5      = ``format "%+.5d",42''   -=- "+00042"

testIntFixedSigned5 = ``format "%+10.5i",42'' -=- "    +00042"

-- Format a charater:
ex3 :: Char -> String
ex3 c = ``format "This is a char: %c",c''

testEx3 = (ex3 'a') -=- "This is a char: a"

-- Integer and character formatting:
testIntFixedSignedChar =  ``format "%+5d%c",42,c''  -=-  "  +42%"
  where
   c = '%'                           

-- Format a string with a given width and maximal length:
ex4 :: String -> String
ex4 s = ``format "This is a string: %010.4s!",s''

testEx4 = (ex4 "Hello") -=- "This is a string:       Hell!"

-- Format with passing expressions:
ex8 :: Int -> Int -> String
ex8 n1 n2 = ``format "The sum of %+.5d and %+5i is %+6i.\n",n1,n2,n1+n2''

test_intexp = (ex8 42 2143) -=- "The sum of +00042 and +2143 is  +2185.\n"

-- Format a float with a given width and precision:
testFloatFixPrec3 = ``format "%+8.3f",f'' -=- "  +3.142"
 where
  f = 3.14159
  
-- Format a float with an exponent:
testFloatExp = ``format "% .4E",f'' -=- " 3.1416E+02"
 where
  f = 314.159

-- Format the sum of two integers:
ex7 :: Int -> Int -> String
ex7 n1 n2 = ``format "The sum of %+.5d and %+5i is %+6i",n1,n2,n1+n2''

testEx7 = (ex7 1234 890) -=- "The sum of +01234 and  +890 is  +2124"
