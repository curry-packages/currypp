{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

import Test.EasyCheck

-- Operation to delete suffixes of the fornm ".0"
fix_int (s++".0") = s
fix_int'default s = s

main = map fix_int ["1.3","1.0","42"]

test1 = fix_int "1.3" -=- "1.3"
test2 = fix_int "1.0" -=- "1"
test3 = fix_int "42"  -=- "42"
