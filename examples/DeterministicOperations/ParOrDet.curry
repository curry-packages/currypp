{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=defaultrules #-}

import Control.SetFunctions -- required by deterministic functions
import Test.Prop

-- Parallel or declared as a deterministic function:
por :: Bool -> Bool -> DET Bool
por True  _     = True
por _     True  = True
por False False = False

main = por True True

test_por1 = por True  True  -=- True
test_por2 = por True  False -=- True
test_por3 = por False True  -=- True
test_por4 = por False False -=- False
