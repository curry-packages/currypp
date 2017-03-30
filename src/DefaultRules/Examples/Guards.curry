{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}
{-# OPTIONS_CYMAKE -Wnone #-}

import Test.EasyCheck

-- Examples for multiple rules with guards:

-- Non-deterministic guard: the non-determinism is encapsulated
zero x | x<0 ? x>0 = "Not zero"
zero'default x = "Zero"

main1 = map zero [-1, 0, 1]

testZero1 = zero (-1) -=- "Not zero"
testZero2 = zero  0   -=- "Zero"
testZero3 = zero  1   -=- "Not zero"


f True _    z | z <= 1 = 1
f _    True z | z > -1 = 2
f'default _ _ _ = 3

main2 = [f True  True  (-1)
        ,f True  False (-1)
        ,f True  True  0
        ,f True  False 0
        ,f True  True  2
        ,f False True  0
        ,f False True  2
        ]

testf1 = f True  True  (-1) -=- 1
testf2 = f True  False (-1) -=- 1
testf3 = f True  True  0    <~> (1 ? 2)
testf4 = f True  False 0    -=- 1
testf5 = f True  True  2    -=- 2
testf6 = f False True  0    -=- 2
testf7 = f False True  2    -=- 2
testf8 = f False False 0    -=- 3

g x | x==0 = 0
    | x==1 = 1
g'default x = 2
