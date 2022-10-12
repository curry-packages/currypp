{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
--- This program contains some examples for using different kinds of
--- integrated code in a Curry program.
------------------------------------------------------------------------------

import RegExpEff
import Data.Format

isEmail :: String -> Bool
isEmail = match ``regex
  [a-zA-Z0-9]([a-zA-Z0-9\._])*
  @
  [a-zA-Z0-9][a-zA-Z0-9\-]*\.
  ([:alnum:][a-zA-Z0-9\-]*\.)*
  [a-zA-Z]{2,4}''

isURL :: String -> Bool
isURL = match ``regex [a-zA-Z0-9][a-zA-Z0-9\-]*\.
                      ([:alnum:][a-zA-Z0-9\-]*\.)*
                      [a-zA-Z]{2,4}''

isListBetween :: String -> Char -> Char -> Bool
isListBetween s min max = match ``regex [<min>-<max>]*'' s

printList :: [Int] -> IO ()
printList []     = return ()
printList (e:es) = putStr ``format "%+20.4d\n",e''
                   >> printList es

printFl :: Float -> IO ()
printFl f = ``printf "% 20.4f\n",f''

printEl :: Float -> IO ()
printEl f = do ``printf "%+20.3E",f'' >> putStr " meters\n"
               putStrLn "DONE!"
