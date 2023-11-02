{-# OPTIONS_FRONTEND -F --pgmF=currypp --optF=contracts #-}

import Control.Search.SetFunctions
import Test.Contract
import Test.Prop

-- Examples with non-determinististic specifications.
-- Note that due to the postconditions which are automatically generated
-- from the specifications, the transformed programs contains unintended
-- uses of set functions so that it should not be checked with
-- source checks by CurryCheck.

coin'spec :: Int
coin'spec = 0 ? 1

coin :: Int
coin = 1 ? 0   --> should be executed without violation

coinCorrect = coin <~> coin'spec


coin3'spec :: Int
coin3'spec = coin'spec

coin3 :: Int
coin3 = coin ? 2  --> should produce a violation

coin3Violation = toError coin3
