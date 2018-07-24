{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=foreigncode #-}

------------------------------------------------------------------------------
--- This program contains some examples for using different kinds of
--- integrated code in a Curry program.
------------------------------------------------------------------------------

import RegExp
import Format

isEmail :: String -> Bool
isEmail s = s ``regex
  [a-zA-Z0-9]([a-zA-Z0-9\._])*
  @
  [a-zA-Z0-9][a-zA-Z0-9\-]*\.
  ([:alnum:][a-zA-Z0-9\-]*\.)*
  [a-zA-Z]{2,4}''

isURL :: String -> Bool
isURL s = s ``regex [a-zA-Z0-9][a-zA-Z0-9\-]*\.
                    ([:alnum:][a-zA-Z0-9\-]*\.)*
                    [a-zA-Z]{2,4}''

isListBetween :: String -> Char -> Char -> Bool
isListBetween l min max = l ``regex [<min>-<max>]*''

printList :: [Int] -> IO ()
printList []     = return ()
printList (e:es) = putStr ``format "%+20.4d\n",e''
                   >> printList es

printFl :: Float -> IO ()
printFl f = ``printf "% 20.4f\n",f''

printEl :: Float -> IO ()
printEl f = do ``printf "%+20.3E",f'' >> putStr " meters\n"
               putStrLn "DONE!"
