{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=seqrules #-}

import Test.EasyCheck

-- The following task should be solved:
-- Break a Curry main expression into an expression and a where...free clause.
-- If the where clause is not present, the returned where-part is empty.

testExps = ["3+4","xs++ys =:= [1,2] where xs,ys free"]

-- FLP solution with default rules:
breakWhereFreeFLP (exp++wf@(" where "++_++" free")) = (exp,wf)
breakWhereFreeFLP exp = (exp,"")

main = map breakWhereFreeFLP testExps

test_without_where = breakWhereFreeFLP "3+4" -=- ("3+4",[])

test_with_where = breakWhereFreeFLP "xs++ys =:= [1,2] where xs,ys free"
                  -=- ("xs++ys =:= [1,2]"," where xs,ys free")