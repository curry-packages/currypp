{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
--- This program contains some tests for integrated code to format strings.
---
--- The format specification C specification for `printf` formatting.
--- This specification may be found at
--- <http://pubs.opengroup.org/onlinepubs/009695399/functions/fprintf.html>
------------------------------------------------------------------------------

import Format -- required in the pre-processed program
import Test.EasyCheck

-- Format a string and an integer:
ex1 :: String -> Int -> String
ex1 name age = ``format "Hello %s. Age %i!",name,age''

test_ex1 = (ex1 "World" 42) -=- "Hello World. Age 42!"

-- Various integer formats:
test_intsigned       = ``format "%+i",42''     -=- "+42"

test_int5            = ``format "%.5i",42''    -=- "00042"

test_intsigned5      = ``format "%+.5d",42''   -=- "+00042"

test_intfixedsigned5 = ``format "%+10.5i",42'' -=- "    +00042"

-- Integer and character formatting:
test_intfixedsignedchar =  ``format "%+5d%c",42,c''  -=-  "  +42%"
  where
   c = '%'                           

-- Format a string with a given width and maximal length:
test_stringlength =
  ``format "This is a string: %08.4s",s''  -=-  "This is a string:     Hell"
 where
  s = "Hello!"
  
-- Format with passing expressions:
ex8 :: Int -> Int -> String
ex8 n1 n2 = ``format "The sum of %+.5d and %+5i is %+6i.\n",n1,n2,n1+n2''

test_intexp = (ex8 42 2143) -=- "The sum of +00042 and +2143 is  +2185.\n"

-- Format a float with a given width and precision:
test_floatfixprec3 = ``format "%+8.3f",f'' -=- "  +3.142"
 where
  f = 3.14159
  
-- Format a float with an exponent:
test_floatexp = ``format "% .4E",f'' -=- " 3.1416E+02"
 where
  f = 314.159