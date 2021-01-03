{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=defaultrules --optF=-o #-}
{-# OPTIONS_FRONTEND -Wnone #-}

-- This example shows that optimal evaluation is still possible
-- with default rules.

f 0 1 = 1
f _ 2 = 2
f'default _ x = x

loop = loop

-- Note this a finite search space is only obtained with KiCS2!
main = [f loop 2, f loop 3]
