{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
--- This program contains some tests for integrated code to support
--- regular expression matching.
--- The syntax of regular expressions is similar to
--- POSIX extended regular expressions
------------------------------------------------------------------------------

import RegExp -- required in the pre-processed program
import Test.EasyCheck

test_abc = ("abc" ``regex abc'')  -=- True

test_abastarc = ("abaaaaaaaaaaaaac" ````regex aba*c'''')  -=- True

test_a_bcstar_plus = ("aabcccaba" ``regex (a|(bc*))+'')   -=- True

test_alpha_1 = ("a" ``regex [:alpha:]'')                  -=- True

test_alpha_2 = (not ("4" ``regex [:alpha:]''))            -=- True

test_alpha_star_1 = ("Abc" ``regex [:alpha:]*'')          -=- True

test_alpha_star_2 = ("ab9c" ``regex [:alpha:]*'')         -=- False

test_a_z_plus_1 = ("abc" ``regex [a-z]+'')                -=- True

test_a_z_plus_2 = ("Abc" ``regex [a-z]+'')                -=- False

-- Examples with parameterized regular expressions:

pregexp1 :: [a] -> a -> a -> Bool
pregexp1 s v1 v2 = s ``regex [<v1>-<v2>]*''

test_para_a_c_1 = pregexp1 "abccba" 'a' 'c'               -=- True

test_para_a_c_2 = pregexp1 "abcdcba" 'a' 'c'              -=- False

pregexp2 :: [a] -> a -> a -> Bool
pregexp2 s v1 v2 = s ``regex (<v1>|<v2>)*''

test_para_0_1_star_1 = pregexp2 [0,1,1,0,0] 0 1           -=- True

test_para_0_1_star_2 = pregexp2 [0,1,2,0,0] 0 1           -=- False

-- A regular expression containing a complex Curry expression:
test_complexexp = ("a" ``regex <((\x -\> x) 'a')>'')      -=- True


-- Email address matching:
isEmail :: String -> Bool
isEmail s = s ``regex
  [a-zA-Z0-9]([a-zA-Z0-9\._])*
  @
  [a-zA-Z0-9][a-zA-Z0-9\-]*\.
  ([:alnum:][a-zA-Z0-9\-]*\.)*
  [a-zA-Z]{2,4}''

test_Email1 = isEmail "pakcs@curry-language.org"      -=- True

test_Email2 = isEmail "pa%kcs@curry-language.org"     -=- False


isID :: String -> Bool
isID s = s ``regex [a-zA-Z][a-zA-Z0-9_']*''

test_ID1 = isID "ab_4'aux"    -=- True

test_ID2 = isID "4ab"         -=- False

