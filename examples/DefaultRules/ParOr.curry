{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=defaultrules #-}
{-# OPTIONS_FRONTEND -Wnone #-}

import Control.SetFunctions
import Test.Prop

-- Parallel or with default rules:
por True  _     = True
por _     True  = True
por'default _ _ = False

main = [por x y | x <- [True,False], y <- [True,False]]

test_por1 = por True  True  <~> True
test_por2 = por True  False -=- True
test_por3 = por False True  -=- True
test_por4 = por False False -=- False
